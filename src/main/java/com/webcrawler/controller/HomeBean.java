package com.webcrawler.controller;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;

import org.hibernate.exception.GenericJDBCException;
import org.hibernate.exception.JDBCConnectionException;
import org.jsoup.Connection;
import org.jsoup.Connection.Request;
import org.jsoup.Connection.Response;
import org.jsoup.UnsupportedMimeTypeException;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;
import org.openqa.selenium.OutputType;
import org.openqa.selenium.TakesScreenshot;
import org.openqa.selenium.WebDriver;
import org.primefaces.context.RequestContext;

import com.webcrawler.dao.JmeterTransControllerTbl;
import com.webcrawler.dao.JmeterTransControllerTblHome;
import com.webcrawler.dao.RequestResponseTbl;
import com.webcrawler.dao.RequestResponseTblHome;
import com.webcrawler.dao.RunIdentTbl;
import com.webcrawler.dao.RunIdentTblHome;
import com.webcrawler.model.UrlProperty;
import com.webcrawler.util.Constants;
import com.webcrawler.util.CrawlUtil;
import com.webcrawler.util.DateUtil;
import com.webcrawler.util.RequestResponseUtil;
import com.webcrawler.util.ScreenShotUtil;
import com.webcrawler.util.Util;

import crawlercommons.robots.BaseRobotRules;

@ManagedBean(name = "homeBean")
// @ViewScoped
@SessionScoped
public class HomeBean implements Serializable {

	private String error;
	private String targetUrl;
	private String iterationPerPage;
	private String runName;
	private Boolean downloadImages;
	private String runTime = "00:00:00";
	private Integer pagesMapped = 0;
	private Boolean hasStarted;
	private Boolean hasFinished;
	private Date startTime;
	
	// Duplicate Run variables
	private String duplicateError;
	private String removalRunName;
	private String percentage;
	private String runTimeRemoval = "00:00:00";
	private Integer pagesMappedRemoval = 0;
	private Boolean hasStartedRemoval;
	private Boolean hasFinishedRemoval;
	private Date startTimeRemoval;
	
	private WebDriver driver;

	private RunIdentTblHome runIdentTblHome = new RunIdentTblHome();
	private RequestResponseTblHome requestResponseTblHome = new RequestResponseTblHome();
	private JmeterTransControllerTblHome jmeterTransControllerTblHome = new JmeterTransControllerTblHome();

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

	public String getIterationPerPage() {
		return iterationPerPage;
	}

	public void setIterationPerPage(String iterationPerPage) {
		this.iterationPerPage = iterationPerPage;
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

	private void validate() {

		if (Util.isNullOrEmpty(this.runName)) {
			this.error += "Run name cannot be empty<br/>";
		}

		if (Util.isNullOrEmpty(this.targetUrl)) {
			this.error += "Target url cannot be empty<br/>";
		}

		if (Util.isNullOrEmpty(this.iterationPerPage)) {
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
		}

	}

	public void start() {

		this.error = "";
		this.pagesMapped = 0;
		this.runTime = "00:00:00";

		this.validate();

		if (Util.isNotNullAndEmpty(this.error)) {
			return;
		}

		Integer iterations = Integer.parseInt(this.iterationPerPage);

		// Get Run Name entry
		RunIdentTbl runIdentTbl = new RunIdentTbl();

		runIdentTbl.setRunIdentifier(this.runName);

		List<RunIdentTbl> runIdentTbls = null;

		try {
			runIdentTbls = this.runIdentTblHome.findByExample(runIdentTbl);
		} catch (JDBCConnectionException e) {
			this.error = "Unable to connect to database<br/>";
		}
		
		if (runIdentTbls != null && Boolean.FALSE.equals(runIdentTbls.isEmpty())) {
			this.error += "Run Name already exists<br/>";
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

		// Do web crawling here

		Queue<UrlProperty> urlProperties = new LinkedList<>();
		// TODO: Remove parsedLinks list if not significantly used
		Queue<RequestResponseTbl> parsedLinks = new LinkedList<>();

		UrlProperty baseUrlProperty = new UrlProperty();

		baseUrlProperty.setName(this.targetUrl);
		baseUrlProperty.setToPageLevel(0);

		urlProperties.add(baseUrlProperty);

		String baseDomain = null;
		try {
			baseDomain = CrawlUtil.getDomainName(this.targetUrl);
		} catch (Exception e1) {
			this.error += "Unable to fetch domain of the website<br/>";
			return;
		}
		
		// Start Time
		this.startTime = Calendar.getInstance().getTime();
		
		this.hasStarted = Boolean.TRUE;
		this.hasFinished = Boolean.FALSE;
		
		Integer count = 0;
		
		if(this.driver == null) {
			this.driver = ScreenShotUtil.initFireFox();
		}
		
		while (Boolean.TRUE.equals(this.hasStarted) && Boolean.FALSE.equals(urlProperties.isEmpty())) {

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

				List<RequestResponseTbl> tempRequestResponseTbls = this.requestResponseTblHome
						.findByExample(tempRequestResponseTbl);

				Integer iterationNumer = 0;

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
				}
				
				Connection connection = RequestResponseUtil.makeRequest(urlProperty);

				Document htmlDocument = connection.get();

				Document lastHtmlDocument = urlProperty.getHtmlDocument();
				Response lastResponse = urlProperty.getLastReponse();
				Request lastRequest = urlProperty.getLastRequest();

				Response response = connection.response();
				Request request = connection.request();

				// 200 is the HTTP OK status code
				if (response.statusCode() == 200) {
					
					Boolean isWithinDomain = CrawlUtil.isWithinDomain(this.targetUrl,
							RequestResponseUtil.refectorUrl(response.url().toString(), baseDomain));

					if (Boolean.FALSE.equals(isWithinDomain) || (Boolean.FALSE.equals(response.contentType().contains("text/html"))
							&& Boolean.FALSE.equals(response.contentType().contains("application/xhtml+xml")))) {
						continue;
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

					}

					requestResponseTbl.setPageTransitionIterationNumber(iterationNumer);
					requestResponseTbl.setRequestHeader(request.headers().toString());
					requestResponseTbl.setResponseBody(response.body());
					requestResponseTbl.setResponseHeader(response.headers().toString());
					requestResponseTbl.setToPageLevel(urlProperty.getToPageLevel());

					UrlProperty matchedUrlProperty = null;
					String title = "";

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

					this.requestResponseTblHome.attachDirty(requestResponseTbl);
					
					try {
						
						RequestResponseTbl requestResponseTblLastest = (RequestResponseTbl) this.requestResponseTblHome.findByExample(requestResponseTbl).get(0);
							
						if (this.driver != null) {
							
							this.driver.get(response.url().toString());
							
							JmeterTransControllerTbl jmeterTransControllerTbl = new JmeterTransControllerTbl();
							
							jmeterTransControllerTbl.setRequestResponseTbl(requestResponseTblLastest);
							jmeterTransControllerTbl.setScreenShot(((TakesScreenshot)this.driver).getScreenshotAs(OutputType.BYTES));
							
							this.jmeterTransControllerTblHome.attachDirty(jmeterTransControllerTbl);
						}
						
					} catch(Exception e) {
						
					}
					
					parsedLinks.add(requestResponseTbl);

					Elements links = htmlDocument.select("a[href]");

					for (Element link : links) {

						String refectoredUrl = RequestResponseUtil.refectorUrl(link.attr("href"), baseDomain);

						isWithinDomain = CrawlUtil.isWithinDomain(this.targetUrl,
								RequestResponseUtil.refectorUrl(refectoredUrl, baseDomain));

						Boolean isAllowed = CrawlUtil.isAllowed(rules, this.targetUrl, refectoredUrl);

						if (Boolean.TRUE.equals(isWithinDomain) && Boolean.TRUE.equals(isAllowed) && Constants.MAX_DEPTH > urlProperty.getToPageLevel()) {

							UrlProperty newUrlProperty = new UrlProperty();

							newUrlProperty.setName(refectoredUrl);
							newUrlProperty.setLastReponse(response);
							newUrlProperty.setLastRequest(connection.request());
							newUrlProperty.setHtmlDocument(htmlDocument);
							newUrlProperty.setLastTitle(title);
							newUrlProperty.setTitleModified(matchedUrlProperty != null);
							newUrlProperty.setToPageLevel(urlProperty.getToPageLevel() + 1);

							urlProperties.add(newUrlProperty);
						}
					}

					// logger.info("**Visiting** Received web page at " + url);
				} else {
					System.err.println("Connection Error - status code : " + response.statusCode());
					// logger.error("**Failure** Web page not recieved at " +
					// url);
				}

			} catch (UnsupportedMimeTypeException e) {
				// No need to log
			} catch (IOException e) {
				// Ignore
			} catch (GenericJDBCException e) {
				// Ignore
			} catch (IllegalStateException e) {
//				System.err.println(e);
				RequestContext reqCtx = RequestContext.getCurrentInstance();
				reqCtx.execute("poll.stop();");
			} catch (JDBCConnectionException e) {
				this.error = "Unable to connect to database";
			} catch (Exception e) {

			}

		}

		Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(this.startTime,
				Calendar.getInstance().getTime());

		this.runTime = hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":"
				+ hoursMinutesSeconds.get("seconds");

		this.hasStarted = Boolean.FALSE;
		this.hasFinished = Boolean.TRUE;
		
		ScreenShotUtil.killFirefox();
	}

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
			runIdentTbls = this.runIdentTblHome.findByExample(runIdentTbl);
		} catch (JDBCConnectionException e) {
			this.duplicateError = "Unable to connect to database<br/>";
		}

		if (runIdentTbls == null || Boolean.TRUE.equals(runIdentTbls.isEmpty())) {
			this.duplicateError += "Run Name not found<br/>";
		} else {
			runIdentTbl = runIdentTbls.get(0);

			// Check if run Name percentage is greater or not
			if (Boolean.TRUE.equals(runIdentTbl.getCleansed()) && percentageValue <= runIdentTbl.getPercent()) {
				this.duplicateError += "Already cleansed with " + runIdentTbl.getPercent() + "%<br/>";
			}
		}
		
		if (Util.isNotNullAndEmpty(this.duplicateError)) {
			return;
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

				if (Boolean.FALSE.equals(requestResponseTbl.getId().equals(innerRequestResponseTbl.getId()))) {

					Integer distance = CrawlUtil.levenshteinDistance(requestResponseTbl.getResponseBody(), innerRequestResponseTbl.getResponseBody());

					Double percentage = 100 - (((double) distance)
							/ (Math.max(requestResponseTbl.getResponseBody().length(), innerRequestResponseTbl.getResponseBody().length()))) * 100;

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
		
		if(Boolean.FALSE.equals(hasFinishedRemoval)) {
			// Completely cleansed
			
			runIdentTbl.setCleansed(Boolean.TRUE);
			
			this.runIdentTblHome.attachDirty(runIdentTbl);
			
		}
		
		this.hasStartedRemoval = Boolean.FALSE;
		this.hasFinishedRemoval = Boolean.TRUE;
	}

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
			
			/*ScreenShotUtil.killFirefox();*/

		} else {
			this.pagesMappedRemoval = 0;
			this.runTimeRemoval = "00:00:00";
		}

		this.hasStartedRemoval = Boolean.FALSE;
		this.hasFinishedRemoval = Boolean.TRUE;
	}

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

}
