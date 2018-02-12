package com.webcrawler.controller;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import javax.faces.bean.ManagedBean;
import javax.faces.view.ViewScoped;

import org.hibernate.exception.GenericJDBCException;
import org.hibernate.exception.JDBCConnectionException;
import org.jsoup.Connection;
import org.jsoup.Connection.KeyVal;
import org.jsoup.Connection.Request;
import org.jsoup.Connection.Response;
import org.jsoup.UnsupportedMimeTypeException;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.openqa.selenium.Cookie;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.primefaces.context.RequestContext;

import com.webcrawler.common.controller.AbstractController;
import com.webcrawler.common.util.DateUtil;
import com.webcrawler.common.util.Util;
import com.webcrawler.dao.CredsTbl;
import com.webcrawler.dao.JmeterTransControllerTbl;
import com.webcrawler.dao.RequestResponseTbl;
import com.webcrawler.dao.RunIdentTbl;
import com.webcrawler.dao.RunIdentTblHome;
import com.webcrawler.jmeter.handler.RecordingHandler;
import com.webcrawler.model.AuthenticationForm;
import com.webcrawler.model.UrlProperty;
import com.webcrawler.parser.XmlParser;
import com.webcrawler.util.AuthUtil;
import com.webcrawler.util.Constants;
import com.webcrawler.util.CorrelationUtil;
import com.webcrawler.util.CrawlUtil;
import com.webcrawler.util.DataUtil;
import com.webcrawler.util.FileUtil;
import com.webcrawler.util.RequestResponseUtil;
import com.webcrawler.util.ScreenShotUtil;

import crawlercommons.robots.BaseRobotRules;

@ManagedBean(name = "crawlerController")
@ViewScoped
public class CrawlerController extends AbstractController {
	
	private String fullDomain;
	private String baseDomain;
	private Integer count = 0;
	private BaseRobotRules rules = null;
	private RunIdentTbl runIdentTbl = new RunIdentTbl();
	private Boolean isLoggedIn = Boolean.FALSE;
	private Map<String, String> authCookies = null;
	private Queue<UrlProperty> urlProperties = new LinkedList<>();
	
	/**
	 * The method validate() is use to validate the input for web-crawling
	 */
	private void validate() {
		
		CrawlerBean crawlerBean = getCrawlerBean();
		
		if (Util.isNullOrEmpty(crawlerBean.getRunName())) {
			crawlerBean.setError("Run name cannot be empty<br/>");;
		}

		if (Util.isNullOrEmpty(crawlerBean.getTargetUrl())) {
			crawlerBean.setError("Target url cannot be empty<br/>");
		}
		
		if(Boolean.TRUE.equals(crawlerBean.getAssociateUserCredentials())) {
			if(Util.isNullOrEmpty(crawlerBean.getUserCredentialFilePath())) {
				crawlerBean.setError("User Credential File Path cannot be empty<br/>");
			} else {
				// Check if file exists
				Boolean isFileExists = FileUtil.isFileExists(crawlerBean.getUserCredentialFilePath());
				
				if(Boolean.FALSE.equals(isFileExists)) {
					crawlerBean.setError("File does not eixt<br/>");
				}
			}
		}
	}
	
	public void start() {
		
		CrawlerBean crawlerBean = getCrawlerBean();
		
		crawlerBean.clear();

		this.validate();

		if (Util.isNotNullAndEmpty(crawlerBean.getError())) {
			return;
		}
		
		runIdentTbl.setRunIdentifier(crawlerBean.getRunName());

		List<RunIdentTbl> runIdentTbls = null;
		
		RunIdentTblHome runIdentTblHome = getRunIdentTblHome();

		try {
			runIdentTbls = runIdentTblHome.findByRunName(runIdentTbl.getRunIdentifier());
		} catch (JDBCConnectionException e) {
			crawlerBean.setError("Unable to connect to database<br/>");
		}
		
		Integer count = 0;
		
		if (runIdentTbls != null && Boolean.FALSE.equals(runIdentTbls.isEmpty())) {
			
			runIdentTbl = runIdentTbls.get(0);
			count ++;
			
			for(RunIdentTbl runIdentTblTemp : runIdentTbls) {
			
				if (Boolean.FALSE.equals(runIdentTblTemp.getBaseUrl().toLowerCase().contains(crawlerBean.getTargetUrl().toLowerCase()))
					|| (Boolean.FALSE.equals(crawlerBean.getAssociateUserCredentials()) && Util.isNullOrEmpty(runIdentTblTemp.getAuthFileLoc())
							|| Boolean.TRUE.equals(crawlerBean.getAssociateUserCredentials()) && Util.isNotNullAndEmpty(runIdentTblTemp.getAuthFileLoc()))) {
					crawlerBean.setError("Run Name already exists<br/>");
				}
			}
		}

		if (Util.isNotNullAndEmpty(crawlerBean.getError())) {
			return;
		}

		try {
			rules = CrawlUtil.getWebsiteRules(Constants.USER_AGENT, crawlerBean.getTargetUrl());
		} catch (Exception e) {
			e.printStackTrace();
		}

		if (rules != null && Boolean.TRUE.equals(rules.isAllowNone())) {
			crawlerBean.setError("Webcrawling is not allowed on this site<br/>");
			return;
		}

		UrlProperty baseUrlProperty = new UrlProperty();

		baseUrlProperty.setName(crawlerBean.getTargetUrl());
		baseUrlProperty.setToPageLevel(0);

		urlProperties.add(baseUrlProperty);

		// Get domain name
		try {
			baseDomain = CrawlUtil.getDomainName(crawlerBean.getTargetUrl());
			fullDomain = CrawlUtil.getFullDomainName(crawlerBean.getTargetUrl());
		} catch (Exception e1) {
			crawlerBean.setError("Unable to fetch domain of the website<br/>");
			return;
		}
		
		// Initialize login vars
		isLoggedIn = Boolean.FALSE;
		String username = null;
		String password = null;
		authCookies = null;
		
		// Start Time
		crawlerBean.setStartTime(Calendar.getInstance().getTime());
		
		crawlerBean.setHasStarted(Boolean.TRUE);
		crawlerBean.setHasFinished(Boolean.FALSE);
		
		crawlerBean.setDriver(ScreenShotUtil.initFireFox());
		
		Integer freePort = RecordingHandler.getFreePort();
		
		crawlerBean.setPort(freePort != null ? freePort : Constants.JEMTER_PORT);
		
		// Initialize JMeter
		try {
			crawlerBean.getRecordingHandler().init(crawlerBean.getPort());
		} catch (Exception e) {
		}
		
		if(Boolean.TRUE.equals(crawlerBean.getAssociateUserCredentials())) {
			Map<String, String> usernamePasswordMap = FileUtil.extractUsernamePassword(crawlerBean.getUserCredentialFilePath());
			
			if(usernamePasswordMap == null || Boolean.FALSE.equals(usernamePasswordMap.containsKey("username")) 
					|| Boolean.FALSE.equals(usernamePasswordMap.containsKey("password"))) {
				
				crawlerBean.setError(crawlerBean.getError() + "Unable to fetch username password from given file path<br/>");
				return;
				
			} else if(Util.isNullOrEmpty(crawlerBean.getLoginPageUrl())) {

				crawlerBean.setError(crawlerBean.getError() + "Login Page Url cannot be empty<br/>");
				return;
				
			} else {
				
				// Go for authentication process
				username = usernamePasswordMap.get("username");
				password = usernamePasswordMap.get("password");
				
				AuthUtil.loginPassword = password;
				AuthUtil.loginUsername = username;
			}
			
			// Crawl for 100 times
			crawl(crawlerBean);
			
			if(Util.isNotNullAndEmpty(crawlerBean.getError())) {
				return;
			}
			
			urlProperties.clear();
			
			baseUrlProperty = new UrlProperty();

			baseUrlProperty.setName(crawlerBean.getTargetUrl());
			baseUrlProperty.setToPageLevel(0);

			urlProperties.add(baseUrlProperty);
			
			// Authenticate
			login(crawlerBean, username, password);
			
			if(Util.isNotNullAndEmpty(crawlerBean.getError())) {
				return;
			}
		}

		// Crawl infinitely 
		crawl(crawlerBean);
		
		crawlingFinished();
	}

	private void login(CrawlerBean crawlerBean, String username, String password) {
		
		// Do login
		if(Boolean.TRUE.equals(crawlerBean.getAssociateUserCredentials()) && Boolean.TRUE.equals(crawlerBean.getHasStarted())) {
			
			// Start JMeter recording
			try {
				crawlerBean.getRecordingHandler().stop();
				crawlerBean.setPort(crawlerBean.getRecordingHandler().start(crawlerBean.getPort()));
			} catch (Exception e) {
				// Don't worry if recording not started. Proceed normally
			}
			
			UrlProperty urlProperty = new UrlProperty();
			
			urlProperty.setName(crawlerBean.getLoginPageUrl());
			
			Connection loginForm = RequestResponseUtil.makeRequest(urlProperty, crawlerBean.getPort(), Boolean.FALSE, null);
			
			try {
				Document lastHtmlDocument = loginForm.get();
				Response response = loginForm.response();
				Request request = loginForm.request();
				
				// Duplicate Request Response logging in db
				
				// 200 is the HTTP OK status code
				if (response.statusCode() == 200) {
					
					runIdentTbl.setBaseUrl(crawlerBean.getTargetUrl());
					runIdentTbl.setAuthFileLoc(crawlerBean.getUserCredentialFilePath());
					
					getRunIdentTblHome().attachDirty(runIdentTbl);
					
					List<RunIdentTbl> runIdentTbls = getRunIdentTblHome().findByExample(runIdentTbl);
					
					if (runIdentTbls == null && runIdentTbls.isEmpty()) {
						crawlerBean.setError(crawlerBean.getError() + "Run Name Not saved in database<br/>");
						crawlingFinished();
						
						return;
					}
					
					runIdentTbl = runIdentTbls.get(0);
					
					count ++;
					
					// Get Sampler Proxy Controller as XML
					String jmxHashTree = crawlerBean.getRecordingHandler().stop();
					String transformedSamplerProxy = "";
					
					if(jmxHashTree != null) {
					
						BufferedReader br = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream("/httpsamplerproxy-transformer.xslt")));
						
						transformedSamplerProxy = XmlParser.parseXmlWithXslTransformer(jmxHashTree, br);
						
						try {
							br.close();
						} catch(Exception e) {
							// Do nothing
						}
					}
					
					crawlerBean.setPagesMapped(crawlerBean.getPagesMapped() + 1);
					
					System.out.println("Connected to (GET) : " + urlProperty.getName());
					
					RequestResponseTbl requestResponseTbl = new RequestResponseTbl();
					
					if (lastHtmlDocument != null) {
						String title = "";

						Elements elems = lastHtmlDocument.head().getElementsByTag("title");

						for (Element elem : elems) {
							title += elem.html();
						}

						requestResponseTbl.setFromPageTitle(title);
					}

					if (response != null) {
						requestResponseTbl.setFromPageUrl(response.url().toString());
					}

					Map<String, String> requestHeaders = new HashMap<>(request.headers());
					
					requestHeaders.putAll(request.cookies());
					
					for(KeyVal keyVal : request.data()) {
						requestHeaders.put(keyVal.key(), keyVal.value());
					}
					
					Map<String, String> responseHeaders = new HashMap<>(request.headers());
					
					if(Util.isNotNull(request.cookies())) {
						responseHeaders.putAll(response.cookies());
					}
					
					responseHeaders.putAll(response.headers());
					
					if(urlProperty.getAuthForm() != null && urlProperty.getAuthForm().getData() != null) {
						requestResponseTbl.setRequestParameters(urlProperty.getAuthForm().getData().toString());
					}
					
					requestResponseTbl.setRequestHeader(requestHeaders.toString());
					requestResponseTbl.setRequestHeader(request.headers().toString());
					requestResponseTbl.setResponseBody(response.body());
					requestResponseTbl.setResponseHeader(responseHeaders.toString());
					requestResponseTbl.setToPageLevel(urlProperty.getToPageLevel());

					String title = "";
					
					// Extract and make title
					if (lastHtmlDocument != null) {

						Elements elems = lastHtmlDocument.head().getElementsByTag("title");

						for (Element elem : elems) {
							title += elem.html();
						}

						requestResponseTbl.setToPageTitle(title);
					}
					
					if(count == 0) {
						runIdentTbl.setBaseUrl(crawlerBean.getTargetUrl());
						runIdentTbl.setAuthFileLoc(crawlerBean.getUserCredentialFilePath());
						
						getRunIdentTblHome().attachDirty(runIdentTbl);
						
						runIdentTbls = getRunIdentTblHome().findByExample(runIdentTbl);
						
						if (runIdentTbls == null && runIdentTbls.isEmpty()) {
							crawlerBean.setError(crawlerBean.getError() + "Run Name Not saved in database<br/>");
							return;
						}
						
						runIdentTbl = runIdentTbls.get(0);
						
						count ++;
					}
					
					requestResponseTbl.setToPageUrl(urlProperty.getName());
					requestResponseTbl.setRunIdentTbl(runIdentTbl);
					requestResponseTbl.setAuthenticated(Boolean.TRUE.equals(isLoggedIn) ? 1 : 0);

					getRequestResponseTblHome().attachDirty(requestResponseTbl);
					
					// Take and save screen shot. Also save Jmeter output
					try {
						
						JmeterTransControllerTbl jmeterTransControllerTbl = new JmeterTransControllerTbl();
						
						RequestResponseTbl requestResponseTblLastest = (RequestResponseTbl) getRequestResponseTblHome().findByExample(requestResponseTbl).get(0);
						
						Map<String, String> headers = new LinkedHashMap(CorrelationUtil.extractHeaders(requestHeaders.toString(), DataUtil.getIgnoreHeaderKeys()));
						
						jmeterTransControllerTbl.setRequestResponseTbl(requestResponseTblLastest);
						jmeterTransControllerTbl.setTransContSec(XmlParser.parseRequestHeaderXmlAndUpdateValues(transformedSamplerProxy, headers));
//						jmeterTransControllerTbl.setTransContSec(transformedSamplerProxy);
						
						if (crawlerBean.getDriver() != null) {
							try {
								
								crawlerBean.getDriver().get(response.url().toString());

								jmeterTransControllerTbl.setScreenShot(
										((TakesScreenshot) crawlerBean.getDriver()).getScreenshotAs(OutputType.BYTES));
							} catch (Exception e) {

							}
						}
						
						getJmeterTransControllerTblHome().attachDirty(jmeterTransControllerTbl);
						
					} catch(Exception e) {
						
					}
					
				} else {
					// Because response is not 200
					crawlerBean.setError(crawlerBean.getError() + "Unable to login<br/>");
					crawlingFinished();
					
					return;
				}
				
				// login
				// Start JMeter recording
				try {
					crawlerBean.getRecordingHandler().stop();
					crawlerBean.setPort(crawlerBean.getRecordingHandler().start(crawlerBean.getPort()));
				} catch (Exception e) {
					// Don't worry if recording not started. Proceed normally
				}
				
				AuthenticationForm authForm = AuthUtil.findAndFillForm(lastHtmlDocument);
	
				if (authForm == null) {
					crawlerBean.setError("Unable to find login form<br />");
					crawlingFinished();
					
					return;
				}
	
				urlProperty.setAuthForm(authForm);
				urlProperty.setLastReponse(response);
				urlProperty.setLastRequest(request);
	
				Connection connection = RequestResponseUtil.makeRequest(urlProperty, crawlerBean.getPort(), Boolean.TRUE, null);

				Document htmlDocument = connection.post();

				Boolean isSuccessfulLogin = AuthUtil.isLoginSuccessful(htmlDocument, authForm);

				if (Boolean.FALSE.equals(isSuccessfulLogin)) {
					crawlerBean.setError("Unable to login<br />");
					crawlingFinished();
					
					return;
				}

				isLoggedIn = Boolean.TRUE;
				if(response.cookies() != null) {
					authCookies = response.cookies();
					authCookies.putAll(connection.response().cookies());
				} else {
					authCookies = connection.response().cookies();
				}
				
				UrlProperty baseUrlProp = urlProperties.poll();
				
				if(baseUrlProp != null) {
					baseUrlProp.setLastReponse(connection.response());
					baseUrlProp.setLastRequest(connection.request());
				}
				
				urlProperties.add(baseUrlProp);
				
				// TODO: Check Jmeter and db instructions
				
				// 200 is the HTTP OK status code
				if (response.statusCode() == 200) {
					
					// Get Sampler Proxy Controller as XML
					String jmxHashTree = crawlerBean.getRecordingHandler().stop();
					String transformedSamplerProxy = "";
					
					if(jmxHashTree != null) {
					
						BufferedReader br = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream("/httpsamplerproxy-transformer-post-method.xslt")));
						
						transformedSamplerProxy = XmlParser.parseXmlWithXslTransformer(jmxHashTree, br);
						
						try {
							br.close();
						} catch(Exception e) {
							// Do nothing
						}
					}
					
					crawlerBean.setPagesMapped(crawlerBean.getPagesMapped() + 1);
					
					System.out.println("Connected to (POST) : " + urlProperty.getName());
					
					RequestResponseTbl requestResponseTbl = new RequestResponseTbl();
					
					if (lastHtmlDocument != null) {
						String title = "";

						Elements elems = lastHtmlDocument.head().getElementsByTag("title");

						for (Element elem : elems) {
							title += elem.html();
						}

						requestResponseTbl.setFromPageTitle(title);
					}

					if (response != null) {
						requestResponseTbl.setFromPageUrl(response.url().toString());
					}

					Map<String, String> headers = new HashMap<>(request.headers());
					
					headers.putAll(request.cookies());
					
					for(KeyVal keyVal : request.data()) {
						headers.put(keyVal.key(), keyVal.value());
					}
					
					if(urlProperty.getAuthForm() != null && urlProperty.getAuthForm().getData() != null) {
						requestResponseTbl.setRequestParameters(urlProperty.getAuthForm().getData().toString());
					}
					
					Map<String, String> responseHeaders = new HashMap<>(request.headers());
					
					if(Util.isNotNull(request.cookies())) {
						responseHeaders.putAll(response.cookies());
					}
					
					responseHeaders.putAll(response.headers());
					
					requestResponseTbl.setRequestHeader(headers.toString());
					requestResponseTbl.setRequestHeader(request.headers().toString());
					requestResponseTbl.setResponseBody(response.body());
					requestResponseTbl.setResponseHeader(responseHeaders.toString());
					requestResponseTbl.setToPageLevel(urlProperty.getToPageLevel());

					String title = "";
					
					// Extract and make title
					if (htmlDocument != null) {

						Elements elems = htmlDocument.head().getElementsByTag("title");

						for (Element elem : elems) {
							title += elem.html();
						}

						requestResponseTbl.setToPageTitle(title);
					}
					
					if(count == 0) {
						runIdentTbl.setBaseUrl(crawlerBean.getTargetUrl());
						runIdentTbl.setAuthFileLoc(crawlerBean.getUserCredentialFilePath());
						
						getRunIdentTblHome().attachDirty(runIdentTbl);
						
						List<RunIdentTbl> runIdentTbls = getRunIdentTblHome().findByExample(runIdentTbl);
						
						if (runIdentTbls == null && runIdentTbls.isEmpty()) {
							crawlerBean.setError(crawlerBean.getError() + "Run Name Not saved in database<br/>");
							return;
						}
						
						runIdentTbl = runIdentTbls.get(0);
						
						count ++;
					}
					
					requestResponseTbl.setToPageUrl(urlProperty.getName());
					requestResponseTbl.setRunIdentTbl(runIdentTbl);
					requestResponseTbl.setAuthenticated(Boolean.TRUE.equals(isLoggedIn) ? 1 : 0);

					getRequestResponseTblHome().attachDirty(requestResponseTbl);
					
					// Save username and password for this run
					CredsTbl credsTbl = new CredsTbl();
					
					credsTbl.setRunIdentTbl(runIdentTbl);
					credsTbl.setUsername(username);
					credsTbl.setUsernameVariable(Constants.USERNAME_NICKNAME);
					credsTbl.setPassword(password);
					credsTbl.setPasswordVariable(Constants.PASSWORD_NICKNAME);
					
					crawlerBean.getCredsTblHome().attachDirty(credsTbl);
					
					// TODO: Update transformedSamplerProxy and add request parameters
					
					// Take and save screen shot. Also save Jmeter output
					try {
						
						JmeterTransControllerTbl jmeterTransControllerTbl = new JmeterTransControllerTbl();
						
						RequestResponseTbl requestResponseTblLastest = (RequestResponseTbl) crawlerBean.getRequestResponseTblHome().findByExample(requestResponseTbl).get(0);
						
						Map<String, String> filteredHeaders = new LinkedHashMap(CorrelationUtil.extractHeaders(headers.toString(), DataUtil.getIgnoreHeaderKeys()));
						
						jmeterTransControllerTbl.setRequestResponseTbl(requestResponseTblLastest);
						jmeterTransControllerTbl.setTransContSec(XmlParser.parseRequestHeaderXmlAndUpdateValues(transformedSamplerProxy, filteredHeaders));
//						jmeterTransControllerTbl.setTransContSec(transformedSamplerProxy);
						
						if (crawlerBean.getDriver() != null) {
							try {
								
								crawlerBean.getDriver().get(response.url().toString());

								jmeterTransControllerTbl.setScreenShot(
										((TakesScreenshot) crawlerBean.getDriver()).getScreenshotAs(OutputType.BYTES));
							} catch (Exception e) {

							}
						}
						
						getJmeterTransControllerTblHome().attachDirty(jmeterTransControllerTbl);
						
					} catch(Exception e) {
						
					}
					
				} else {
					// Because response is not 200 i.e. OK
					crawlerBean.setError(crawlerBean.getError() + "Unable to login<br/>");
					crawlingFinished();
					
					return;
				}

			} catch (Exception e) {
				crawlerBean.setError("Unable to login<br />");
				crawlingFinished();
				
				return;
			}
		}
	}
	
	private Queue<UrlProperty> crawl(CrawlerBean crawlerBean) {
		
		Integer iterations = 1;
		Integer pageTraversed = 0;
		
		// Start web crawling here
		while (Boolean.TRUE.equals(crawlerBean.getHasStarted()) && Boolean.FALSE.equals(urlProperties.isEmpty())
				&& (Boolean.FALSE.equals(crawlerBean.getAssociateUserCredentials()) || 
						(Boolean.TRUE.equals(this.isLoggedIn) || pageTraversed < Constants.PAGES_CRAWL_BEFORE_AUTHENTICATION))) {
			// Start JMeter recording
			try {
				crawlerBean.getRecordingHandler().stop();
				crawlerBean.setPort(crawlerBean.getRecordingHandler().start(crawlerBean.getPort()));
			} catch (Exception e) {
				// Don't worry if recording not started. Proceed normally
			}
			
			try {
		
				UrlProperty urlProperty = urlProperties.poll();
		
				RequestResponseTbl tempRequestResponseTbl = new RequestResponseTbl();
		
				String fromUrl = null;
		
				if (urlProperty.getLastRequest() != null) {
					fromUrl = urlProperty.getLastRequest().url().toString();
					tempRequestResponseTbl.setFromPageUrl(fromUrl);
				}
		
				String toUrl = urlProperty.getName();
		
				tempRequestResponseTbl.setToPageUrl(toUrl);
				// tempRequestResponseTbl.setRunIdentTbl(runIdentTbl);
				
				// TODO: Uncomment this line and comment out below line
				List<RequestResponseTbl> tempRequestResponseTbls = getRequestResponseTblHome()
						.findByExample(toUrl, fromUrl, runIdentTbl.getId(), Boolean.TRUE.equals(isLoggedIn) ? 1 : 0);
				//TODO: Check if there is any issue with auth = 1/0 ...
				/*List<RequestResponseTbl> tempRequestResponseTbls = this.requestResponseTblHome
						.findByExample(toUrl, null, runIdentTbl.getId(), Boolean.TRUE.equals(isLoggedIn) ? 1 : 0);*/
				
				Integer iterationNumer = 1;
				
				if (tempRequestResponseTbls.size() >= iterations) {
					continue;
				} else {
					iterationNumer = tempRequestResponseTbls.size() + 1;
				}
		
				Document lastHtmlDocument = urlProperty.getHtmlDocument();
				Document htmlDocument = null;
				Connection connection = null;
				
				// Make request
				connection = RequestResponseUtil.makeRequest(urlProperty, crawlerBean.getPort(), Boolean.FALSE, authCookies);
				// Call HttpGet
				htmlDocument = connection.get();
					
				Response lastResponse = urlProperty.getLastReponse();
				Request lastRequest = urlProperty.getLastRequest();
		
				Response response = connection.response();
				Request request = connection.request();
				
				// 200 is the HTTP OK status code
				if (response.statusCode() == 200) {
					
					Boolean isWithinDomain = CrawlUtil.isWithinDomain(crawlerBean.getTargetUrl(),
							RequestResponseUtil.refectorUrl(response.url().toString(), baseDomain));
					
					// Check if response is valid 
					if (Boolean.FALSE.equals(isWithinDomain) || (Boolean.FALSE.equals(response.contentType().contains("text/html"))
							&& Boolean.FALSE.equals(response.contentType().contains("application/xhtml+xml")))) {
						continue;
					}
					
					pageTraversed++;
		
					// Get Sampler Proxy Controller as XML
					String jmxHashTree = crawlerBean.getRecordingHandler().stop();
					String transformedSamplerProxy = "";
					
					if(jmxHashTree != null) {
					
						BufferedReader br = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream("/httpsamplerproxy-transformer.xslt")));
						
						transformedSamplerProxy = XmlParser.parseXmlWithXslTransformer(jmxHashTree, br);
						
						try {
							br.close();
						} catch(Exception e) {
							// Do nothing
						}
					}
					
					crawlerBean.setPagesMapped(crawlerBean.getPagesMapped() + 1);
		
					System.out.println("Connected to : " + urlProperty.getName());
		
					RequestResponseTbl requestResponseTbl = new RequestResponseTbl();
		
					if (lastHtmlDocument != null) {
						String title = "";
		
						Elements elems = lastHtmlDocument.head().getElementsByTag("title");
		
						for (Element elem : elems) {
							title += elem.html();
						}
		
						requestResponseTbl.setFromPageTitle(title);
					}
		
					if (lastResponse != null) {
						requestResponseTbl.setFromPageUrl(lastResponse.url().toString());
					}
		
					if (lastRequest != null) {
						// Do nothing
					}
		
					Map<String, String> requestHeaders = new HashMap<>(request.headers());
					
					requestHeaders.putAll(request.cookies());
					
					for(KeyVal keyVal : request.data()) {
						requestHeaders.put(keyVal.key(), keyVal.value());
					}
					
					Map<String, String> responseHeaders = new HashMap<>(request.headers());
					
					if(Util.isNotNull(request.cookies())) {
						responseHeaders.putAll(response.cookies());
					}
					
					responseHeaders.putAll(response.headers());
					
					requestResponseTbl.setRequestHeader(requestHeaders.toString());
//					requestResponseTbl.setPageTransitionIterationNumber(iterationNumer);
					requestResponseTbl.setResponseBody(response.body());
					requestResponseTbl.setResponseHeader(responseHeaders.toString());
					requestResponseTbl.setToPageLevel(urlProperty.getToPageLevel());
		
					UrlProperty matchedUrlProperty = null;
					String title = "";
					
					// Extract and make title
					if (htmlDocument != null) {
		
						Elements elems = htmlDocument.head().getElementsByTag("title");
		
						for (Element elem : elems) {
							title += elem.html();
						}
		
						requestResponseTbl.setToPageTitle(title);
					}
					
					if(count == 0) {
						runIdentTbl.setBaseUrl(crawlerBean.getTargetUrl());
						runIdentTbl.setAuthFileLoc(null);
						
						getRunIdentTblHome().attachDirty(runIdentTbl);
						
						List<RunIdentTbl> runIdentTbls = getRunIdentTblHome().findByExample(runIdentTbl);
						
						if (runIdentTbls == null || runIdentTbls.isEmpty()) {
							crawlerBean.setError(crawlerBean.getError() + "Run Name Not saved in database<br/>");
							break;
						}
						
						runIdentTbl = runIdentTbls.get(runIdentTbls.size() -1);
						
						count ++;
					}
					
					requestResponseTbl.setToPageUrl(urlProperty.getName());
					requestResponseTbl.setRunIdentTbl(runIdentTbl);
					requestResponseTbl.setAuthenticated(Boolean.TRUE.equals(isLoggedIn) ? 1 : 0);
		
					getRequestResponseTblHome().attachDirty(requestResponseTbl);
					
					// Take and save screen shot. Also save Jmeter output
					try {
						
						JmeterTransControllerTbl jmeterTransControllerTbl = new JmeterTransControllerTbl();
						
						RequestResponseTbl requestResponseTblLastest = (RequestResponseTbl) getRequestResponseTblHome().findByExample(requestResponseTbl).get(0);
						
						Map<String, String> headers = new LinkedHashMap(CorrelationUtil.extractHeaders(requestHeaders.toString(), DataUtil.getIgnoreHeaderKeys()));
						
						jmeterTransControllerTbl.setRequestResponseTbl(requestResponseTblLastest);
						jmeterTransControllerTbl.setTransContSec(XmlParser.parseRequestHeaderXmlAndUpdateValues(transformedSamplerProxy, headers));
//						jmeterTransControllerTbl.setTransContSec(transformedSamplerProxy);
						
						if (crawlerBean.getDriver() != null) {
							try {
								
								if(authCookies != null) {
									Set<String> keys = authCookies.keySet();
									
									for(String key : keys) {
										crawlerBean.getDriver().manage().addCookie(new Cookie(key, authCookies.get(key)));
									}
								}
								
								if(CrawlUtil.canTakeScreenShot(response.url().toString())) {
									crawlerBean.getDriver().get(response.url().toString());
									
									jmeterTransControllerTbl.setScreenShot(
											((TakesScreenshot) crawlerBean.getDriver()).getScreenshotAs(OutputType.BYTES));
								}
								
							} catch (Exception e) {
		
							}
						}
						
						getJmeterTransControllerTblHome().attachDirty(jmeterTransControllerTbl);
						
					} catch(Exception e) {
						
					}
					
					Elements links = htmlDocument.select("a[href]");
					
					// Extract all links that will be called in next iteration
					for (Element link : links) {
						
						String refectoredUrl = RequestResponseUtil.refectorUrl(link.attr("href"), fullDomain);
		
						isWithinDomain = CrawlUtil.isWithinDomain(crawlerBean.getTargetUrl(),
								RequestResponseUtil.refectorUrl(refectoredUrl, fullDomain));
		
						Boolean isAllowed = CrawlUtil.isAllowed(rules, crawlerBean.getTargetUrl(), refectoredUrl);
						
						// Check if it is achieved MAX_DEPTH = vertical-depth
						if (Boolean.TRUE.equals(isWithinDomain) && Boolean.TRUE.equals(isAllowed) && Constants.MAX_DEPTH > urlProperty.getToPageLevel()) {
		
							UrlProperty newUrlProperty = new UrlProperty();
		
							newUrlProperty.setName(refectoredUrl);
							newUrlProperty.setLastReponse(response);
							newUrlProperty.setLastRequest(connection.request());
							newUrlProperty.setHtmlDocument(htmlDocument);
							newUrlProperty.setLastTitle(title);
							newUrlProperty.setTitleModified(matchedUrlProperty != null);
							newUrlProperty.setToPageLevel(urlProperty.getToPageLevel() + 1);
		
							if(Boolean.TRUE.equals(isLoggedIn) && Boolean.TRUE.equals(AuthUtil.isLogoutLink(link))) {
								// Check if link signout link or not. If signout then do nothing
							} else {
								urlProperties.add(newUrlProperty);
							}
						}
					}
		
				} else {
					System.err.println("Connection Error - status code : " + response.statusCode());
				}
		
			} catch (UnsupportedMimeTypeException e) {
				// No need to log, ignore
			} catch (IOException e) {
				// Ignore
			} catch (GenericJDBCException e) {
				// Ignore
			} catch (IllegalStateException e) {
				RequestContext reqCtx = RequestContext.getCurrentInstance();
				reqCtx.execute("poll.stop();");
			} catch (JDBCConnectionException e) {
				crawlerBean.setError("Unable to connect to database");
			} catch (Exception e) {
				// Ignore
			}
		}
		
		return urlProperties;
	}
	
	private void crawlingFinished() {
		
		CrawlerBean crawlerBean = getCrawlerBean();
		
		// Stop timing
		Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(crawlerBean.getStartTime(),
				Calendar.getInstance().getTime());

		crawlerBean.setRunTime(hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":"
				+ hoursMinutesSeconds.get("seconds"));

		crawlerBean.setHasStarted(Boolean.FALSE);
		crawlerBean.setHasFinished(Boolean.TRUE);
	
		// Kill firefox
		ScreenShotUtil.killFirefox();
	}

	/**
	 * The method stop() is use to stop web-crarwling
	 */
	public void stop() {
		
		CrawlerBean crawlerBean = getCrawlerBean();
		
		if (Boolean.TRUE.equals(crawlerBean.getHasStarted())) {
			// Calculate time
			Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(crawlerBean.getStartTime(),
					Calendar.getInstance().getTime());

			crawlerBean.setRunTime(hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":"
					+ hoursMinutesSeconds.get("seconds"));

			/*
			 * RequestContext reqCtx = RequestContext.getCurrentInstance();
			 * reqCtx.execute("poll.stop();");
			 */
			
			/*ScreenShotUtil.killFirefox();*/

		} else {
			crawlerBean.setPagesMapped(0);
			crawlerBean.setRunTime("00:00:00");
		}

		crawlerBean.setHasStarted(Boolean.FALSE);
		crawlerBean.setHasFinished(Boolean.TRUE);
	}

	/**
	 * The method fetchUpdates() is use to fetch updated time and pags parsed
	 */
	public void fetchUpdates() {
		
		CrawlerBean crawlerBean = getCrawlerBean();
		
		if (Boolean.TRUE.equals(crawlerBean.getHasStarted())) {
			// Calculate time
			Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(crawlerBean.getStartTime(),
					Calendar.getInstance().getTime());

			crawlerBean.setRunTime(hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":"
					+ hoursMinutesSeconds.get("seconds"));

		} else if (Boolean.FALSE.equals(crawlerBean.getHasFinished())) {
			crawlerBean.setPagesMapped(0);
			crawlerBean.setRunTime("00:00:00");
		}
	}
}
