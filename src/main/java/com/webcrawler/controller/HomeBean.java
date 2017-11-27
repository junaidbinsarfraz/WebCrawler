package com.webcrawler.controller;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Serializable;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;

import org.hibernate.exception.GenericJDBCException;
import org.hibernate.exception.JDBCConnectionException;
import org.jsoup.Connection;
import org.jsoup.Connection.KeyVal;
import org.jsoup.Connection.Request;
import org.jsoup.Connection.Response;
import org.jsoup.Jsoup;
import org.jsoup.UnsupportedMimeTypeException;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.openqa.selenium.Cookie;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.primefaces.context.RequestContext;
import org.w3c.dom.Node;

import com.webcrawler.dao.CredsTbl;
import com.webcrawler.dao.CredsTblHome;
import com.webcrawler.dao.HeaderCorrelationTbl;
import com.webcrawler.dao.HeaderCorrelationTblHome;
import com.webcrawler.dao.JmeterTransControllerTbl;
import com.webcrawler.dao.JmeterTransControllerTblHome;
import com.webcrawler.dao.KnownExtractorsTbl;
import com.webcrawler.dao.PageCategoryTbl;
import com.webcrawler.dao.RequestCorrelationTbl;
import com.webcrawler.dao.RequestCorrelationTblHome;
import com.webcrawler.dao.RequestResponseTbl;
import com.webcrawler.dao.RequestResponseTblHome;
import com.webcrawler.dao.RunIdentTbl;
import com.webcrawler.dao.RunIdentTblHome;
import com.webcrawler.jmeter.handler.RecordingHandler;
import com.webcrawler.jmeter.util.XmlParser;
import com.webcrawler.model.AuthenticationForm;
import com.webcrawler.model.UrlProperty;
import com.webcrawler.util.AuthUtil;
import com.webcrawler.util.Constants;
import com.webcrawler.util.CorrelationUtil;
import com.webcrawler.util.CrawlUtil;
import com.webcrawler.util.DataUtil;
import com.webcrawler.util.DateUtil;
import com.webcrawler.util.DocumentParser;
import com.webcrawler.util.FileUtil;
import com.webcrawler.util.RequestResponseUtil;
import com.webcrawler.util.ScreenShotUtil;
import com.webcrawler.util.Util;

import crawlercommons.robots.BaseRobotRules;

/**
 * The class HomeBean is used as a controller. It gets all the request and show
 * the response. It also manage all the activities like initialize firefox,
 * web-crawling, screenshots, removal of duplicates, start/stop web-crawling,
 * start/stop duplicate removals, save/delete database operations.
 * 
 * @author Junaid
 */
@ManagedBean(name = "homeBean")
// @ViewScoped
@SessionScoped
public class HomeBean implements Serializable {

	private String error;
	private String targetUrl;
	private String runName;
	private Boolean downloadImages;
	private String runTime = "00:00:00";
	private Integer pagesMapped = 0;
	private Boolean hasStarted;
	private Boolean hasFinished;
	private Date startTime;
	private Boolean associateUserCredentials;
	private String userCredentialFilePath;
	private String loginPageUrl;
	
	// Duplicate Run variables
	private String duplicateError;
	private String removalRunName;
	private String percentage;
	private String runTimeRemoval = "00:00:00";
	private Integer pagesMappedRemoval = 0;
	private Boolean hasStartedRemoval;
	private Boolean hasFinishedRemoval;
	private Date startTimeRemoval;
	
	// Correlation Run variables 
	private String correlationRunName;
	private String correlationError;
	private String correlationStatus;
	
	private WebDriver driver;
	private RecordingHandler recordingHandler = new RecordingHandler();

	private RunIdentTblHome runIdentTblHome = new RunIdentTblHome();
	private RequestResponseTblHome requestResponseTblHome = new RequestResponseTblHome();
	private JmeterTransControllerTblHome jmeterTransControllerTblHome = new JmeterTransControllerTblHome();
	private CredsTblHome credsTblHome = new CredsTblHome();
	private HeaderCorrelationTblHome headerCorrelationTblHome = new HeaderCorrelationTblHome();
	private RequestCorrelationTblHome requestCorrelationTblHome = new RequestCorrelationTblHome();
	
	// Local variables
	private Integer port;

	public String getError() {
		return error;
	}

	public void setError(String error) {
		this.error = error;
	}

	public String getTargetUrl() {
		return targetUrl;
	}

	public void setTargetUrl(String targetUrl) {
		this.targetUrl = targetUrl;
	}

	public String getRunName() {
		return runName;
	}

	public void setRunName(String runName) {
		this.runName = runName;
	}

	public Boolean getDownloadImages() {
		return downloadImages;
	}

	public void setDownloadImages(Boolean downloadImages) {
		this.downloadImages = downloadImages;
	}

	public String getRunTime() {
		return runTime;
	}

	public void setRunTime(String runTime) {
		this.runTime = runTime;
	}

	public Integer getPagesMapped() {
		return pagesMapped;
	}

	public void setPagesMapped(Integer pagesMapped) {
		this.pagesMapped = pagesMapped;
	}

	public Boolean getHasStarted() {
		return hasStarted;
	}

	public void setHasStarted(Boolean hasStarted) {
		this.hasStarted = hasStarted;
	}

	public Boolean getHasFinished() {
		return hasFinished;
	}

	public void setHasFinished(Boolean hasFinished) {
		this.hasFinished = hasFinished;
	}
	
	public Date getStartTime() {
		return startTime;
	}

	public void setStartTime(Date startTime) {
		this.startTime = startTime;
	}

	public Boolean getAssociateUserCredentials() {
		return associateUserCredentials;
	}

	public void setAssociateUserCredentials(Boolean associateUserCredentials) {
		this.associateUserCredentials = associateUserCredentials;
	}

	public String getUserCredentialFilePath() {
		return userCredentialFilePath;
	}

	public void setUserCredentialFilePath(String userCredentialFilePath) {
		this.userCredentialFilePath = userCredentialFilePath;
	}

	public String getLoginPageUrl() {
		return loginPageUrl;
	}

	public void setLoginPageUrl(String loginPageUrl) {
		this.loginPageUrl = loginPageUrl;
	}

	public String getDuplicateError() {
		return duplicateError;
	}

	public void setDuplicateError(String duplicateError) {
		this.duplicateError = duplicateError;
	}

	public String getRemovalRunName() {
		return removalRunName;
	}

	public void setRemovalRunName(String removalRunName) {
		this.removalRunName = removalRunName;
	}

	public String getPercentage() {
		return percentage;
	}

	public void setPercentage(String percentage) {
		this.percentage = percentage;
	}

	public String getRunTimeRemoval() {
		return runTimeRemoval;
	}

	public void setRunTimeRemoval(String runTimeRemoval) {
		this.runTimeRemoval = runTimeRemoval;
	}

	public Integer getPagesMappedRemoval() {
		return pagesMappedRemoval;
	}

	public void setPagesMappedRemoval(Integer pagesMappedRemoval) {
		this.pagesMappedRemoval = pagesMappedRemoval;
	}

	public Boolean getHasStartedRemoval() {
		return hasStartedRemoval;
	}

	public void setHasStartedRemoval(Boolean hasStartedRemoval) {
		this.hasStartedRemoval = hasStartedRemoval;
	}

	public Boolean getHasFinishedRemoval() {
		return hasFinishedRemoval;
	}

	public void setHasFinishedRemoval(Boolean hasFinishedRemoval) {
		this.hasFinishedRemoval = hasFinishedRemoval;
	}

	public Date getStartTimeRemoval() {
		return startTimeRemoval;
	}

	public void setStartTimeRemoval(Date startTimeRemoval) {
		this.startTimeRemoval = startTimeRemoval;
	}

	public String getCorrelationRunName() {
		return correlationRunName;
	}

	public void setCorrelationRunName(String correlationRunName) {
		this.correlationRunName = correlationRunName;
	}

	public String getCorrelationError() {
		return correlationError;
	}

	public void setCorrelationError(String correlationError) {
		this.correlationError = correlationError;
	}

	public String getCorrelationStatus() {
		return correlationStatus;
	}

	public void setCorrelationStatus(String correlationStatus) {
		this.correlationStatus = correlationStatus;
	}

	/**
	 * The method validate() is use to validate the input for web-crawling
	 */
	private void validate() {

		if (Util.isNullOrEmpty(this.runName)) {
			this.error += "Run name cannot be empty<br/>";
		}

		if (Util.isNullOrEmpty(this.targetUrl)) {
			this.error += "Target url cannot be empty<br/>";
		}
		
		if(Boolean.TRUE.equals(this.associateUserCredentials)) {
			if(Util.isNullOrEmpty(this.userCredentialFilePath)) {
				this.error += "User Credential File Path cannot be empty<br/>";
			} else {
				// Check if file exists
				Boolean isFileExists = FileUtil.isFileExists(this.userCredentialFilePath);
				
				if(Boolean.FALSE.equals(isFileExists)) {
					this.error += "File does not eixt<br/>";
				}
			}
		}

		/*if (Util.isNullOrEmpty(this.iterationPerPage)) {
			this.error += "Iteration per page cannot be empty<br/>";
		} else if (!Util.isNumber(this.iterationPerPage)) {
			this.error += "Iteration per page must be a number<br/>";
		} else {
			try {
				Integer iterations = Integer.parseInt(this.iterationPerPage);

				if (iterations < 1) {
					this.error += "Iteration per page must be greater then 0<br/>";
				} else if (iterations > 100) {
					this.error += "Iteration per page must not be greater then 100<br/>";
				}
			} catch (Exception e) {
				this.error += "Iteration per page must be an integer<br/>";
			}
		}*/

	}

	/**
	 * The method start() is use to start web-crawling
	 */
	public void start() {
		
		this.error = "";
		this.pagesMapped = 0;
		this.runTime = "00:00:00";

		this.validate();

		if (Util.isNotNullAndEmpty(this.error)) {
			return;
		}

//		Integer iterations = Integer.parseInt(this.iterationPerPage);
		Integer iterations = 1;
		
		// Get Run Name entry
		RunIdentTbl runIdentTbl = new RunIdentTbl();

		runIdentTbl.setRunIdentifier(this.runName);

		List<RunIdentTbl> runIdentTbls = null;

		try {
			runIdentTbls = this.runIdentTblHome.findByRunName(runIdentTbl.getRunIdentifier());
		} catch (JDBCConnectionException e) {
			this.error = "Unable to connect to database<br/>";
		}
		
		Integer count = 0;
		
		if (runIdentTbls != null && Boolean.FALSE.equals(runIdentTbls.isEmpty())) {
			
			runIdentTbl = runIdentTbls.get(0);
			count ++;
			
			for(RunIdentTbl runIdentTblTemp : runIdentTbls) {
			
				if (Boolean.FALSE.equals(runIdentTblTemp.getBaseUrl().toLowerCase().contains(this.targetUrl.toLowerCase()))
					|| (Boolean.FALSE.equals(this.associateUserCredentials) && Util.isNullOrEmpty(runIdentTblTemp.getAuthFileLoc())
							|| Boolean.TRUE.equals(this.associateUserCredentials) && Util.isNotNullAndEmpty(runIdentTblTemp.getAuthFileLoc()))) {
								this.error += "Run Name already exists<br/>";
				}
			}
		}

		if (Util.isNotNullAndEmpty(this.error)) {
			return;
		}

		BaseRobotRules rules = null;

		try {
			rules = CrawlUtil.getWebsiteRules(Constants.USER_AGENT, this.targetUrl);
		} catch (Exception e) {
			e.printStackTrace();
		}

		if (rules != null && Boolean.TRUE.equals(rules.isAllowNone())) {
			this.error += "Webcrawling is not allowed on this site<br/>";
			return;
		}

		Queue<UrlProperty> urlProperties = new LinkedList<>();
		// TODO: Remove parsedLinks list if not significantly used
		Queue<RequestResponseTbl> parsedLinks = new LinkedList<>();

		UrlProperty baseUrlProperty = new UrlProperty();

		baseUrlProperty.setName(this.targetUrl);
		baseUrlProperty.setToPageLevel(0);

		urlProperties.add(baseUrlProperty);

		// Get domain name
		String baseDomain = null;
		String fullDomain = null;
		try {
			baseDomain = CrawlUtil.getDomainName(this.targetUrl);
			fullDomain = CrawlUtil.getFullDomainName(this.targetUrl);
		} catch (Exception e1) {
			this.error += "Unable to fetch domain of the website<br/>";
			return;
		}
		
		// Initialize login vars
		Boolean isLoggedIn = Boolean.FALSE;
		String username = null;
		String password = null;
		Map<String, String> authCookies = null;
		
		// Check login credentials 
		if(Boolean.TRUE.equals(this.associateUserCredentials)) {
			Map<String, String> usernamePasswordMap = FileUtil.extractUsernamePassword(this.userCredentialFilePath);
			
			if(usernamePasswordMap == null || Boolean.FALSE.equals(usernamePasswordMap.containsKey("username")) 
					|| Boolean.FALSE.equals(usernamePasswordMap.containsKey("password"))) {
				
				this.error += "Unable to fetch username password from given file path<br/>";
				return;
				
			} else if(Util.isNullOrEmpty(this.loginPageUrl)) {

				this.error += "Login Page Url cannot be empty<br/>";
				return;
				
			} else {
				
				// Go for authentication process
				username = usernamePasswordMap.get("username");
				password = usernamePasswordMap.get("password");
				
				AuthUtil.loginPassword = password;
				AuthUtil.loginUsername = username;
			}
		}
		
		// Start Time
		this.startTime = Calendar.getInstance().getTime();
		
		this.hasStarted = Boolean.TRUE;
		this.hasFinished = Boolean.FALSE;
		
		/*if(this.driver == null) {
			this.driver = ScreenShotUtil.initFireFox();
		}*/
		
		this.driver = ScreenShotUtil.initFireFox();
		
		Integer freePort = RecordingHandler.getFreePort();
		
		this.port = freePort != null ? freePort : com.webcrawler.jmeter.util.Constants.PORT;
		
		// Initialize JMeter
		try {
			recordingHandler.init(this.port);
		} catch (Exception e) {
		}
		
		// Do login
		if(Boolean.TRUE.equals(this.associateUserCredentials)) {
			
			// Start JMeter recording
			try {
				recordingHandler.stop();
				this.port = recordingHandler.start(this.port);
			} catch (Exception e) {
				// Don't worry if recording not started. Proceed normally
			}
			
			UrlProperty urlProperty = new UrlProperty();
			
			urlProperty.setName(this.loginPageUrl);
			
			Connection loginForm = RequestResponseUtil.makeRequest(urlProperty, this.port, Boolean.FALSE, null);
			
			try {
				Document lastHtmlDocument = loginForm.get();
				Response response = loginForm.response();
				Request request = loginForm.request();
				
				// Duplicate Request Response logging in db
				
				/////////////////////
				
				// 200 is the HTTP OK status code
				if (response.statusCode() == 200) {
					
					runIdentTbl.setBaseUrl(this.targetUrl);
					runIdentTbl.setAuthFileLoc(this.userCredentialFilePath);
					
					this.runIdentTblHome.attachDirty(runIdentTbl);
					
					runIdentTbls = this.runIdentTblHome.findByExample(runIdentTbl);
					
					if (runIdentTbls == null && runIdentTbls.isEmpty()) {
						this.error += "Run Name Not saved in database<br/>";
						crawlingFinished();
						
						return;
					}
					
					runIdentTbl = runIdentTbls.get(0);
					
					count ++;
					
					// Get Sampler Proxy Controller as XML
					String jmxHashTree = recordingHandler.stop();
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
					
					this.pagesMapped++;
					
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
						runIdentTbl.setBaseUrl(this.targetUrl);
						runIdentTbl.setAuthFileLoc(this.userCredentialFilePath);
						
						this.runIdentTblHome.attachDirty(runIdentTbl);
						
						runIdentTbls = this.runIdentTblHome.findByExample(runIdentTbl);
						
						if (runIdentTbls == null && runIdentTbls.isEmpty()) {
							this.error += "Run Name Not saved in database<br/>";
							return;
						}
						
						runIdentTbl = runIdentTbls.get(0);
						
						count ++;
					}
					
					requestResponseTbl.setToPageUrl(urlProperty.getName());
					requestResponseTbl.setRunIdentTbl(runIdentTbl);
					requestResponseTbl.setAuthenticated(Boolean.TRUE.equals(isLoggedIn) ? 1 : 0);

					this.requestResponseTblHome.attachDirty(requestResponseTbl);
					
					// Take and save screen shot. Also save Jmeter output
					try {
						
						JmeterTransControllerTbl jmeterTransControllerTbl = new JmeterTransControllerTbl();
						
						RequestResponseTbl requestResponseTblLastest = (RequestResponseTbl) this.requestResponseTblHome.findByExample(requestResponseTbl).get(0);
						
						jmeterTransControllerTbl.setRequestResponseTbl(requestResponseTblLastest);
						jmeterTransControllerTbl.setTransContSec(transformedSamplerProxy);
						
						if (this.driver != null) {
							try {
								
								this.driver.get(response.url().toString());

								jmeterTransControllerTbl.setScreenShot(
										((TakesScreenshot) this.driver).getScreenshotAs(OutputType.BYTES));
							} catch (Exception e) {

							}
						}
						
						this.jmeterTransControllerTblHome.attachDirty(jmeterTransControllerTbl);
						
					} catch(Exception e) {
						
					}
					
				} else {
					// Because response is not 200
					this.error += "Unable to login<br/>";
					crawlingFinished();
					
					return;
				}
				
				////////////////////
				
				// login
				
				// Start JMeter recording
				try {
					recordingHandler.stop();
					this.port = recordingHandler.start(this.port);
				} catch (Exception e) {
					// Don't worry if recording not started. Proceed normally
				}
				
				AuthenticationForm authForm = AuthUtil.findAndFillForm(lastHtmlDocument);
	
				if (authForm == null) {
					this.error = "Unable to find login form<br />";
					crawlingFinished();
					
					return;
				}
	
				urlProperty.setAuthForm(authForm);
				urlProperty.setLastReponse(response);
				urlProperty.setLastRequest(request);
	
				Connection connection = RequestResponseUtil.makeRequest(urlProperty, this.port, Boolean.TRUE, null);

				Document htmlDocument = connection.post();

				Boolean isSuccessfulLogin = AuthUtil.isLoginSuccessful(htmlDocument, authForm);

				if (Boolean.FALSE.equals(isSuccessfulLogin)) {
					this.error = "Unable to login<br />";
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
					String jmxHashTree = recordingHandler.stop();
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
					
					this.pagesMapped++;
					
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
						runIdentTbl.setBaseUrl(this.targetUrl);
						runIdentTbl.setAuthFileLoc(this.userCredentialFilePath);
						
						this.runIdentTblHome.attachDirty(runIdentTbl);
						
						runIdentTbls = this.runIdentTblHome.findByExample(runIdentTbl);
						
						if (runIdentTbls == null && runIdentTbls.isEmpty()) {
							this.error += "Run Name Not saved in database<br/>";
							return;
						}
						
						runIdentTbl = runIdentTbls.get(0);
						
						count ++;
					}
					
					requestResponseTbl.setToPageUrl(urlProperty.getName());
					requestResponseTbl.setRunIdentTbl(runIdentTbl);
					requestResponseTbl.setAuthenticated(Boolean.TRUE.equals(isLoggedIn) ? 1 : 0);

					this.requestResponseTblHome.attachDirty(requestResponseTbl);
					
					// Save username and password for this run
					CredsTbl credsTbl = new CredsTbl();
					
					credsTbl.setRunIdentTbl(runIdentTbl);
					credsTbl.setUsername(username);
					credsTbl.setUsernameVariable(Constants.USERNAME_NICKNAME);
					credsTbl.setPassword(password);
					credsTbl.setPasswordVariable(Constants.PASSWORD_NICKNAME);
					
					this.credsTblHome.attachDirty(credsTbl);
					
					// TODO: Update transformedSamplerProxy and add request parameters
					
					// Take and save screen shot. Also save Jmeter output
					try {
						
						JmeterTransControllerTbl jmeterTransControllerTbl = new JmeterTransControllerTbl();
						
						RequestResponseTbl requestResponseTblLastest = (RequestResponseTbl) this.requestResponseTblHome.findByExample(requestResponseTbl).get(0);
						
						jmeterTransControllerTbl.setRequestResponseTbl(requestResponseTblLastest);
						jmeterTransControllerTbl.setTransContSec(transformedSamplerProxy);
						
						if (this.driver != null) {
							try {
								
								this.driver.get(response.url().toString());

								jmeterTransControllerTbl.setScreenShot(
										((TakesScreenshot) this.driver).getScreenshotAs(OutputType.BYTES));
							} catch (Exception e) {

							}
						}
						
						this.jmeterTransControllerTblHome.attachDirty(jmeterTransControllerTbl);
						
					} catch(Exception e) {
						
					}
					
				} else {
					// Because response is not 200
					this.error += "Unable to login<br/>";
					crawlingFinished();
					
					return;
				}

			} catch (Exception e) {
				this.error = "Unable to login<br />";
				crawlingFinished();
				
				return;
			}
			
		}
		
		// Start web crawling here
		while (Boolean.TRUE.equals(this.hasStarted) && Boolean.FALSE.equals(urlProperties.isEmpty())) {
			
			// Start JMeter recording
			try {
				recordingHandler.stop();
				this.port = recordingHandler.start(this.port);
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

				/*List<RequestResponseTbl> tempRequestResponseTbls = this.requestResponseTblHome
						.findByExample(tempRequestResponseTbl);

				Integer iterationNumer = 0;

				// Calculate iteration count/number
				if (tempRequestResponseTbls != null && Boolean.FALSE.equals(tempRequestResponseTbls.isEmpty())) {

					List<RequestResponseTbl> requestResponseTbls1 = new ArrayList<>();

					for (RequestResponseTbl tempRequestResponseTbl1 : tempRequestResponseTbls) {
						if (tempRequestResponseTbl1.getRunIdentTbl() != null
								&& tempRequestResponseTbl1.getRunIdentTbl().getId() != null && runIdentTbl != null
								&& tempRequestResponseTbl1.getRunIdentTbl().getId().equals(runIdentTbl.getId())) {
							requestResponseTbls1.add(tempRequestResponseTbl1);
						}
					}

					if (requestResponseTbls1.size() >= iterations) {
						continue;
					} else {
						iterationNumer = requestResponseTbls1.size() + 1;
					}
				} else {
					iterationNumer = 1;
				}*/
				
				// TODO: Uncomment this line and comment out below line
				List<RequestResponseTbl> tempRequestResponseTbls = this.requestResponseTblHome
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
				connection = RequestResponseUtil.makeRequest(urlProperty, this.port, Boolean.FALSE, authCookies);
				// Call HttpGet
				htmlDocument = connection.get();
					
				Response lastResponse = urlProperty.getLastReponse();
				Request lastRequest = urlProperty.getLastRequest();

				Response response = connection.response();
				Request request = connection.request();
				
				// 200 is the HTTP OK status code
				if (response.statusCode() == 200) {
					
					Boolean isWithinDomain = CrawlUtil.isWithinDomain(this.targetUrl,
							RequestResponseUtil.refectorUrl(response.url().toString(), baseDomain));
					
					// Check if response is valid 
					if (Boolean.FALSE.equals(isWithinDomain) || (Boolean.FALSE.equals(response.contentType().contains("text/html"))
							&& Boolean.FALSE.equals(response.contentType().contains("application/xhtml+xml")))) {
						continue;
					}

					// Get Sampler Proxy Controller as XML
					String jmxHashTree = recordingHandler.stop();
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
					
					this.pagesMapped++;

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

						// To page title change base on pages' distance 
						/*if (Boolean.TRUE || (Util.isNotNullAndEmpty(fromUrl) && Util.isNotNullAndEmpty(toUrl) && toUrl.equals(fromUrl))) {

							matchedUrlProperty = this.getMatchedUrlPropertiesByTitle(urlProperties, title);

							if (matchedUrlProperty != null) {

								Integer distance = CrawlUtil.levenshteinDistance(
										matchedUrlProperty.getLastReponse().body(), response.body());

								Double percentage = (((double) distance)
										/ (Math.max(matchedUrlProperty.getLastReponse().body().length(),
												response.body().length())))
										* 100;

								if (percentage != null && percentage > Constants.PERCENTAGE_MATCH_MIN_LIMIT) {
									if (Boolean.TRUE.equals(matchedUrlProperty.getTitleModified())) {
										// Get last underscore
										title += ("_"
												+ (Integer
														.parseInt(matchedUrlProperty.getLastTitle().substring(
																matchedUrlProperty.getLastTitle().lastIndexOf("_") + 1))
														+ 1));
									} else {
										title += "_1";
									}
								}
							}
						}*/

						requestResponseTbl.setToPageTitle(title);
					}
					
					if(count == 0) {
						runIdentTbl.setBaseUrl(this.targetUrl);
						runIdentTbl.setAuthFileLoc(null);
						
						this.runIdentTblHome.attachDirty(runIdentTbl);
						
						runIdentTbls = this.runIdentTblHome.findByExample(runIdentTbl);
						
						if (runIdentTbls == null && runIdentTbls.isEmpty()) {
							this.error += "Run Name Not saved in database<br/>";
							break;
						}
						
						runIdentTbl = runIdentTbls.get(runIdentTbls.size() -1);
						
						count ++;
					}
					
					requestResponseTbl.setToPageUrl(urlProperty.getName());
					requestResponseTbl.setRunIdentTbl(runIdentTbl);
					requestResponseTbl.setAuthenticated(Boolean.TRUE.equals(isLoggedIn) ? 1 : 0);

					this.requestResponseTblHome.attachDirty(requestResponseTbl);
					
					// TODO: Change transformedSamplerProxy with request headers
					
					// Take and save screen shot. Also save Jmeter output
					try {
						
						JmeterTransControllerTbl jmeterTransControllerTbl = new JmeterTransControllerTbl();
						
						RequestResponseTbl requestResponseTblLastest = (RequestResponseTbl) this.requestResponseTblHome.findByExample(requestResponseTbl).get(0);
						
						jmeterTransControllerTbl.setRequestResponseTbl(requestResponseTblLastest);
						jmeterTransControllerTbl.setTransContSec(transformedSamplerProxy);
						
						if (this.driver != null) {
							try {
								
								if(authCookies != null) {
									Set<String> keys = authCookies.keySet();
									
									for(String key : keys) {
										this.driver.manage().addCookie(new Cookie(key, authCookies.get(key)));
									}
								}
								
								if(CrawlUtil.canTakeScreenShot(response.url().toString())) {
									this.driver.get(response.url().toString());
									
									jmeterTransControllerTbl.setScreenShot(
											((TakesScreenshot) this.driver).getScreenshotAs(OutputType.BYTES));
								}
								
							} catch (Exception e) {

							}
						}
						
						this.jmeterTransControllerTblHome.attachDirty(jmeterTransControllerTbl);
						
					} catch(Exception e) {
						
					}
					
					parsedLinks.add(requestResponseTbl);
					
					// Do not get href links of login page b/c login page is only used for login purpose not for navigation purpose
					/*if(Boolean.TRUE.equals(urlProperty.getLoginLink())) {

						urlProperty.setLoginLink(Boolean.FALSE);
						
						urlProperties.add(urlProperty);
						
						continue;
					}*/
					
					Elements links = htmlDocument.select("a[href]");
					
					// Extract all links that will be called in next iteration
					for (Element link : links) {
						
						// TODO: remove this in final version. Just for testing
						/*if(link.attr("href") != null && link.attr("href").contains("?") && !link.attr("href").contains("signin")) {
							continue;
						}*/
						
						String refectoredUrl = RequestResponseUtil.refectorUrl(link.attr("href"), fullDomain);

						isWithinDomain = CrawlUtil.isWithinDomain(this.targetUrl,
								RequestResponseUtil.refectorUrl(refectoredUrl, fullDomain));

						Boolean isAllowed = CrawlUtil.isAllowed(rules, this.targetUrl, refectoredUrl);
						
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
				// No need to log
			} catch (IOException e) {
				// Ignore
//					System.out.println(e);
			} catch (GenericJDBCException e) {
				// Ignore
			} catch (IllegalStateException e) {
//				System.err.println(e);
				RequestContext reqCtx = RequestContext.getCurrentInstance();
				reqCtx.execute("poll.stop();");
			} catch (JDBCConnectionException e) {
				this.error = "Unable to connect to database";
			} catch (Exception e) {
//					System.out.println(e);
			}
		}
		
		crawlingFinished();
	}
	
	private void crawlingFinished() {
		// Stop timing
		Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(this.startTime,
				Calendar.getInstance().getTime());

		this.runTime = hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":"
				+ hoursMinutesSeconds.get("seconds");

		this.hasStarted = Boolean.FALSE;
		this.hasFinished = Boolean.TRUE;
	
		// Kill firefox
		ScreenShotUtil.killFirefox();
	}

	/**
	 * The method stop() is use to stop web-crarwling
	 */
	public void stop() {
		if (Boolean.TRUE.equals(this.hasStarted)) {
			// Calculate time
			Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(this.startTime,
					Calendar.getInstance().getTime());

			this.runTime = hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":"
					+ hoursMinutesSeconds.get("seconds");

			/*
			 * RequestContext reqCtx = RequestContext.getCurrentInstance();
			 * reqCtx.execute("poll.stop();");
			 */
			
			/*ScreenShotUtil.killFirefox();*/

		} else {
			this.pagesMapped = 0;
			this.runTime = "00:00:00";
		}

		this.hasStarted = Boolean.FALSE;
		this.hasFinished = Boolean.TRUE;
	}

	/**
	 * The method fetchUpdates() is use to fetch updated time and pags parsed
	 */
	public void fetchUpdates() {

		if (Boolean.TRUE.equals(this.hasStarted)) {
			// Calculate time
			Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(this.startTime,
					Calendar.getInstance().getTime());

			this.runTime = hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":"
					+ hoursMinutesSeconds.get("seconds");

		} else if (Boolean.FALSE.equals(hasFinished)) {
			this.pagesMapped = 0;
			this.runTime = "00:00:00";
		}

	}

	/**
	 * The method getMatchedUrlPropertiesByTitle() is use to check if whose page
	 * titles are matched with the give title
	 * 
	 * @param urlProperties
	 *            queue of UrlProperties
	 * @param title
	 *            to be checked
	 * @return UrlProperty that is mached else null
	 */
	private UrlProperty getMatchedUrlPropertiesByTitle(Queue<UrlProperty> urlProperties, String title) {

		UrlProperty matchedUrlProperty = null;

		if (Util.isNotNullAndEmpty(title)) {
			for (UrlProperty urlProperty : urlProperties) {
				if (title.equalsIgnoreCase(urlProperty.getLastTitle())) {
					matchedUrlProperty = urlProperty;
				}
			}
		}

		return matchedUrlProperty;
	}
	
	/////////// Duplicate Removal /////////////
	
	/**
	 * The method startRemoval() is use to start duplicate page removal
	 */
	public void startRemoval() {
		
		// Init
		this.pagesMappedRemoval = 0;
		this.runTimeRemoval = "00:00:00";
		this.duplicateError = "";
		Integer percentageValue = 0;
		
		// Validate inputs
		try {
			percentageValue = Integer.parseInt(this.percentage);
			
			if(percentageValue <= 0) {
				this.duplicateError += "Percentage should be a greater then 0<br/>";
			} else if(percentageValue > 100) {
				this.duplicateError += "Percentage should be a less then 100<br/>";
			}
		} catch (Exception e) {
			// error
			this.duplicateError += "Percentage should be a number<br/>";
		}
		
		// Check if run Name exists
		RunIdentTbl runIdentTbl = new RunIdentTbl();

		runIdentTbl.setRunIdentifier(this.removalRunName);

		List<RunIdentTbl> runIdentTbls = null;

		try {
			runIdentTbls = this.runIdentTblHome.findByRunName(runIdentTbl.getRunIdentifier());
		} catch (JDBCConnectionException e) {
			this.duplicateError = "Unable to connect to database<br/>";
		}

		if (runIdentTbls == null || Boolean.TRUE.equals(runIdentTbls.isEmpty())) {
			this.duplicateError += "Run Name not found<br/>";
			return;
		}
		
		if (Util.isNotNullAndEmpty(this.duplicateError)) {
			return;
		}
		
		for(RunIdentTbl runIdentTbl1 : runIdentTbls) {
			runIdentTbl = runIdentTbl1;
			
			// Check if run Name percentage is greater or not
			if (Boolean.TRUE.equals(runIdentTbl.getCleansed()) && percentageValue <= runIdentTbl.getPercent()) {
				this.duplicateError += "Already cleansed with " + runIdentTbl.getPercent() + "%<br/>";
				continue;
			}
			
			runIdentTbl.setPercent(percentageValue);
			
			this.runIdentTblHome.attachDirty(runIdentTbl);
			
			runIdentTbl = this.runIdentTblHome.findById(runIdentTbl.getId());
			
			// Start doing removals
			RequestResponseTbl dummyRequestResponseTbl = new RequestResponseTbl();
			
			dummyRequestResponseTbl.setRunIdentTbl(runIdentTbl);
			
			// Get all parsed pages
			List<RequestResponseTbl> requestResponseTbls = this.requestResponseTblHome.findByRunId(runIdentTbl.getId());
			
			// Start Time
			this.startTimeRemoval = Calendar.getInstance().getTime();
			
			this.hasStartedRemoval = Boolean.TRUE;
			this.hasFinishedRemoval = Boolean.FALSE;
			
			// Then apply bout-force algorithm
			for (Iterator<RequestResponseTbl> iterator = requestResponseTbls.iterator(); iterator.hasNext();) {
				if(Boolean.TRUE.equals(hasFinishedRemoval)) {
					break;
				}
				
				RequestResponseTbl requestResponseTbl = iterator.next();
				
				inner: for (RequestResponseTbl innerRequestResponseTbl : requestResponseTbls) {
					
					if(Boolean.TRUE.equals(hasFinishedRemoval)) {
						break;
					}
	
					if (Boolean.FALSE.equals(requestResponseTbl.getId().equals(innerRequestResponseTbl.getId()))
							&& requestResponseTbl.getAuthenticated() != null && innerRequestResponseTbl.getAuthenticated() != null
							&& requestResponseTbl.getAuthenticated() - innerRequestResponseTbl.getAuthenticated() == 0) { // Both should have same authenticated value
	
						/*Integer distance = CrawlUtil.levenshteinDistance(requestResponseTbl.getResponseBody(), innerRequestResponseTbl.getResponseBody());
	
						Double percentage = 100 - (((double) distance)
								/ (Math.max(requestResponseTbl.getResponseBody().length(), innerRequestResponseTbl.getResponseBody().length()))) * 100;*/
						
						if(requestResponseTbl.getToPageTitle().equalsIgnoreCase(innerRequestResponseTbl.getToPageTitle()) || 
								requestResponseTbl.getToPageUrl().equalsIgnoreCase(innerRequestResponseTbl.getToPageUrl())) {
						
							Double percentage = 100 - (( (double)Math.max(requestResponseTbl.getResponseBody().length(), innerRequestResponseTbl.getResponseBody().length())
									- (double)Math.min(requestResponseTbl.getResponseBody().length(), innerRequestResponseTbl.getResponseBody().length()))
									/ ((requestResponseTbl.getResponseBody().length() + innerRequestResponseTbl.getResponseBody().length()) / 2)) * 100;
		
							if (percentage != null && percentage.intValue() > percentageValue) {
		
								// Remove from database
								this.requestResponseTblHome.delete(requestResponseTbl);
								iterator.remove();
								this.pagesMappedRemoval++;
								break inner;
							}
						}
					}
				}
			}
			
			/*// Check Completely cleansed
			if(Boolean.FALSE.equals(hasFinishedRemoval)) {
				
				runIdentTbl.setCleansed(Boolean.TRUE);
				
				this.runIdentTblHome.attachDirty(runIdentTbl);
				
			}*/
		}
		
		runIdentTbl.setCleansed(Boolean.FALSE.equals(hasFinishedRemoval));
		runIdentTblHome.attachDirty(runIdentTbl);
		
		this.hasStartedRemoval = Boolean.FALSE;
		this.hasFinishedRemoval = Boolean.TRUE;
	}

	/**
	 * The method stopRemoval() is use to stop duplicate page remova;
	 */
	public void stopRemoval() {
		if (Boolean.TRUE.equals(this.hasStartedRemoval)) {
			// Calculate time
			Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(this.startTimeRemoval,
					Calendar.getInstance().getTime());

			this.runTimeRemoval = hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":"
					+ hoursMinutesSeconds.get("seconds");

			/*
			 * RequestContext reqCtx = RequestContext.getCurrentInstance();
			 * reqCtx.execute("poll.stop();");
			 */
			
		} else {
			this.pagesMappedRemoval = 0;
			this.runTimeRemoval = "00:00:00";
		}

		this.hasStartedRemoval = Boolean.FALSE;
		this.hasFinishedRemoval = Boolean.TRUE;
	}

	/**
	 * The method fetchUpdatesRemoval() is use to update the timing and pages removed count
	 */
	public void fetchUpdatesRemoval() {

		if (Boolean.TRUE.equals(this.hasStartedRemoval)) {
			// Calculate time
			Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(this.startTimeRemoval,
					Calendar.getInstance().getTime());

			this.runTimeRemoval = hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":"
					+ hoursMinutesSeconds.get("seconds");

		} else if (Boolean.FALSE.equals(hasFinishedRemoval)) {
			this.pagesMappedRemoval = 0;
			this.runTimeRemoval = "00:00:00";
		}
		
	}
	
	////////////////////////////////////////////// Correlation ////////////////////////////////////
	
	/**
	 * The method startCorrelation() is use to start correlation for run name
	 */
	public void startCorrelation() {
		
		this.correlationError = "";
		this.correlationStatus = "";
		
		// Validate
		if(Util.isNullOrEmpty(this.correlationRunName)) {
			this.correlationError = "Run Name is required";
			return;
		}
		
		// Check if already run exists
		RunIdentTbl runIdentTbl = new RunIdentTbl();
		
		runIdentTbl.setRunIdentifier(this.correlationRunName);
		
		List<RunIdentTbl> runIdentTbls = this.runIdentTblHome.findByRunName(this.correlationRunName);
		
		if(Util.isNullOrEmpty(runIdentTbls)) {
			this.correlationError = "Run Name doesnot exists";
			return;
		}
		
		runIdentTbl = runIdentTbls.get(0);
		
		List<HeaderCorrelationTbl> headerCorrelationTbls = this.headerCorrelationTblHome.findByRunId(runIdentTbl.getId());
		
		if(Util.isNotNullAndEmpty(headerCorrelationTbls)) {
			this.correlationError = "Run Name is already correlated";
			return;
		}
		
		List<RequestCorrelationTbl> requestCorrelationTbls = this.requestCorrelationTblHome.findByRunId(runIdentTbl.getId());
		
		if(Util.isNotNullAndEmpty(requestCorrelationTbls)) {
			this.correlationError = "Run Name is already correlated";
			return;
		}
		
		Map<String, String> requestCorrelations = new HashMap<>();
		Map<String, String> headerCorrelations = new HashMap<>();
		Map<String, String> responseHeaderCorrelations = new HashMap<>();
		Map<String, String> filteredHeaderCorrelations = new HashMap<>();
		Map<String, String> filteredRequestCorrelations = new HashMap<>();
		Map<String, RequestCorrelationTbl> filteredRequestCorrelationObjects = new HashMap<>();
		Map<String, String> filteredResponseHeaderCorrelations = new HashMap<>();
		
		List<RequestResponseTbl> requestResponseTbls = this.requestResponseTblHome.findByRunId(runIdentTbl.getId());
		
		// Get run name's request response table's request header
		for(RequestResponseTbl requestResponseTblTemp : requestResponseTbls) {
			if(requestResponseTblTemp.getJmeterTransControllerTbls() != null && !requestResponseTblTemp.getJmeterTransControllerTbls().isEmpty()) {
				
				Iterator it = requestResponseTblTemp.getJmeterTransControllerTbls().iterator();
				
				// Get run name's jmeter values 
				if(it.hasNext()) {
					JmeterTransControllerTbl jmeterTransControllerTbl = (JmeterTransControllerTbl) it.next();
					
					requestCorrelations.putAll(CorrelationUtil.extractArgunemtNameValue(jmeterTransControllerTbl.getTransContSec()));
				}
			}
			
			headerCorrelations.putAll(CorrelationUtil.extractHeaders(requestResponseTblTemp.getRequestHeader(), DataUtil.getIgnoreHeaderKeys()));
			responseHeaderCorrelations.putAll(CorrelationUtil.extractHeaders(requestResponseTblTemp.getResponseHeader(), DataUtil.getIgnoreHeaderKeys()));
			
			// Set toPageCategory
			requestResponseTblTemp.setToPageCategory(Constants.UNCATEGORIZE_TO_PAGE_CATEGORY_TEXT);
			
			if(Util.isNotNullAndEmpty(requestResponseTblTemp.getToPageTitle())) {
				for(PageCategoryTbl pageCategoryTbl : DataUtil.getPageCategories()) {
					if(Util.isNotNullAndEmpty(pageCategoryTbl.getKeyword()) && 
							requestResponseTblTemp.getToPageTitle().toLowerCase().contains(pageCategoryTbl.getKeyword().toLowerCase())) {
						requestResponseTblTemp.setToPageCategory(pageCategoryTbl.getCategory());
						break;
					}
				}
			}
			
			this.requestResponseTblHome.attachDirty(requestResponseTblTemp);
			
		}
		
		Integer requestCorrelationVariable = 1;
		
		for(Map.Entry<String, String> requestCorrelation : requestCorrelations.entrySet()) {
			
			// Db operation
			RequestCorrelationTbl requestCorrelationTblTemp = new RequestCorrelationTbl();
			
			requestCorrelationTblTemp.setRunIdentTbl(runIdentTbl);
			requestCorrelationTblTemp.setFoundArgName(requestCorrelation.getKey());
			requestCorrelationTblTemp.setFoundArgValue(requestCorrelation.getValue());
			requestCorrelationTblTemp.setVariable("${cID" + String.format("%03d", requestCorrelationVariable++) + "}");
			requestCorrelationTblTemp.setCorrRegex(requestCorrelationTblTemp.getFoundArgName() + Constants.REQUEST_PARAM_CORR_REGEX);
			
			for(KnownExtractorsTbl knownExtractorsTbl : DataUtil.getKnownExtractors()) {
				if(Util.isNotNullAndEmpty(knownExtractorsTbl.getKnownValue()) 
						&& knownExtractorsTbl.getKnownValue().equalsIgnoreCase(requestCorrelationTblTemp.getFoundArgName())) {
					requestCorrelationTblTemp.setCorrRegex(knownExtractorsTbl.getExtractor());
				}
			}
			
			filteredRequestCorrelations.put(requestCorrelationTblTemp.getFoundArgName(), requestCorrelationTblTemp.getVariable());
			filteredRequestCorrelationObjects.put(requestCorrelationTblTemp.getFoundArgName(), requestCorrelationTblTemp);
			
			// Put in database
			this.requestCorrelationTblHome.attachDirty(requestCorrelationTblTemp);
		}
		
		Integer headerCorrelationVariable = 1;
		
		for(Map.Entry<String, String> headerCorrelation : headerCorrelations.entrySet()) {
			
			// Remove duplicate
			if(!requestCorrelations.containsKey(headerCorrelation.getKey())) {
				
//				filteredHeaderCorrelations.put(headerCorrelation.getKey(), headerCorrelation.getValue());
				
				HeaderCorrelationTbl headerCorrelationTblTemp = new HeaderCorrelationTbl();
				
				headerCorrelationTblTemp.setFoundHeaderName(headerCorrelation.getKey());
				headerCorrelationTblTemp.setFoundHeaderValue(headerCorrelation.getValue());
				headerCorrelationTblTemp.setRunIdentTbl(runIdentTbl);
				headerCorrelationTblTemp.setVariable("${hID" + String.format("%03d", headerCorrelationVariable++) + "}");
				headerCorrelationTblTemp.setCorrRegex(headerCorrelation.getKey() + Constants.CORR_REGEX);
				
				filteredHeaderCorrelations.put(headerCorrelation.getKey(), headerCorrelationTblTemp.getVariable());
				
				if(responseHeaderCorrelations.containsKey(headerCorrelationTblTemp.getFoundHeaderName())) {
					filteredResponseHeaderCorrelations.put(headerCorrelationTblTemp.getFoundHeaderName(), headerCorrelationTblTemp.getVariable());
				}
				
				// Put in database
				this.headerCorrelationTblHome.attachDirty(headerCorrelationTblTemp);
			}
		}
		
//		List<Node> regexExtractors = XmlParser.createRegexExtractors(filteredHeaderCorrelations);
//		List<Node> regexExtractors = XmlParser.createRegexExtractors(filteredResponseHeaderCorrelations);
		
		for(RequestResponseTbl requestResponseTblTemp : requestResponseTbls) {
			if(requestResponseTblTemp.getJmeterTransControllerTbls() != null && !requestResponseTblTemp.getJmeterTransControllerTbls().isEmpty()) {
				Iterator it = requestResponseTblTemp.getJmeterTransControllerTbls().iterator();
				
				// Get run name's jmeter values 
				if(it.hasNext()) {
					
					JmeterTransControllerTbl jmeterTransControllerTbl = (JmeterTransControllerTbl) it.next();
					
					if((requestResponseTblTemp.getAuthenticated() == 1)) {
						
						Map<String, String> tempResponseHeaders = CorrelationUtil.extractHeaders(requestResponseTblTemp.getResponseHeader(), DataUtil.getIgnoreHeaderKeys());
						Map<String, String> finalResponseHeaders = new HashMap<>();
						
						for(Map.Entry<String, String> responseHeader : filteredResponseHeaderCorrelations.entrySet()) {
							if(tempResponseHeaders.containsKey(responseHeader.getKey())) {
								finalResponseHeaders.put(responseHeader.getKey() + Constants.CORR_REGEX, responseHeader.getValue());
							}
						}
						
						List<Node> regexExtractors = XmlParser.createRegexExtractors(finalResponseHeaders, Boolean.TRUE);
						
						// update jmx value with header Correlation values
						jmeterTransControllerTbl.setTransContSec(XmlParser.parseRequestHeaderXmlAndUpdateValues(jmeterTransControllerTbl.getTransContSec(), 
								Util.isNullOrEmpty(requestResponseTblTemp.getRequestParameters()) ? filteredHeaderCorrelations : new HashMap<String, String>(), regexExtractors));
						
						if(Util.isNotNullAndEmpty(requestResponseTblTemp.getRequestParameters())) {
							// TODO: update jmx value with request Correlation values
							List<CredsTbl> credsTbls = credsTblHome.findByRunId(runIdentTbl.getId());
							
							// update jmx parameter values with request correlation values
							if(Util.isNotNullAndEmpty(credsTbls)) {
								jmeterTransControllerTbl.setTransContSec(XmlParser.parseRequestArgumentXmlAndUpdateValues(jmeterTransControllerTbl.getTransContSec(), filteredRequestCorrelations, credsTbls.get(0).getUsername(), credsTbls.get(0).getPassword()));
							}
							
						}
					}
					
					// IT7R5
					String docString = requestResponseTblTemp.getResponseBody();
					
					try {
						Document doc = Jsoup.parse(docString);
						
						List<String> hiddenInputNames = DocumentParser.getHiddenInputName(doc);
						Map<String, String> foundFilteredRequestCorrelations = new HashMap<>();
						
						for(String hiddenInputName : hiddenInputNames) {
							if(Util.isNotNullAndEmpty(hiddenInputName) && filteredRequestCorrelationObjects.containsKey(hiddenInputName)) {
								RequestCorrelationTbl foundRequestCorrelationTbl = filteredRequestCorrelationObjects.get(hiddenInputName);
								foundFilteredRequestCorrelations.put(foundRequestCorrelationTbl.getCorrRegex(), foundRequestCorrelationTbl.getVariable());
							}
						}
						
						List<Node> regexExtractors = XmlParser.createRegexExtractors(foundFilteredRequestCorrelations, Boolean.FALSE);
						
						jmeterTransControllerTbl.setTransContSec(XmlParser.addRequestParametersAsRegexExtractors(jmeterTransControllerTbl.getTransContSec(), regexExtractors));
						
					} catch (Exception e) {
					}
					
					
					this.jmeterTransControllerTblHome.attachDirty(jmeterTransControllerTbl);
				
				}
			}
		}
		
		this.correlationStatus = "Completed";
		
	}

}
