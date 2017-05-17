package com.webcrawler.controller;

import java.io.IOException;
import java.io.Serializable;
import java.net.URISyntaxException;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;

import org.jsoup.Connection;
import org.jsoup.Connection.Request;
import org.jsoup.Connection.Response;
import org.jsoup.UnsupportedMimeTypeException;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import com.webcrawler.dao.RequestResponseTbl;
import com.webcrawler.dao.RequestResponseTblHome;
import com.webcrawler.dao.RunIdentTbl;
import com.webcrawler.dao.RunIdentTblHome;
import com.webcrawler.model.UrlProperty;
import com.webcrawler.util.Constants;
import com.webcrawler.util.CrawlUtil;
import com.webcrawler.util.DateUtil;
import com.webcrawler.util.RequestResponseUtil;
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
	private Date startTime;

	private RunIdentTblHome runIdentTblHome = new RunIdentTblHome();
	private RequestResponseTblHome requestResponseTblHome = new RequestResponseTblHome();

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
	
	private void validate() {
		
		if(Util.isNullOrEmpty(this.runName)) {
			this.error += "Run name cannot be empty<br/>";
		}
		
		if(Util.isNullOrEmpty(this.targetUrl)) {
			this.error += "Target url cannot be empty<br/>";
		}
		
		if(Util.isNullOrEmpty(this.iterationPerPage)) {
			this.error += "Iteration per page cannot be empty<br/>";
		} else if(!Util.isNumber(this.iterationPerPage)) {
			this.error += "Iteration per page must be a number<br/>";
		} else { 
			try {
				Integer iterations = Integer.parseInt(this.iterationPerPage);
				
				if(iterations < 1) {
					this.error += "Iteration per page must be greater then 0<br/>";
				} else if(iterations > 100) {
					this.error += "Iteration per page must not be greater then 100<br/>";
				}
			} catch(Exception e) {
				this.error += "Iteration per page must be an integer<br/>";
			}
		}
		
	}

	public void start() {

		this.error = "";
		this.pagesMapped = 0;
		this.runTime = "00:00:00";
		
		this.validate();
		
		if(Util.isNullOrEmpty(this.error)) {
			return;
		}

		Integer iterations = Integer.parseInt(this.iterationPerPage);

		// Get Run Name entry
		RunIdentTbl runIdentTbl = new RunIdentTbl();

		runIdentTbl.setRunIdentifier(this.runName);

		List<RunIdentTbl> runIdentTbls = this.runIdentTblHome.findByExample(runIdentTbl);

		if (runIdentTbls != null && Boolean.FALSE.equals(runIdentTbls.isEmpty())) {
			this.error += "Run Name already exists\n";
		}

		if (Util.isNotNullAndEmpty(this.error)) {
			return;
		}

		// Start Time
		this.startTime = Calendar.getInstance().getTime();

		this.runIdentTblHome.attachDirty(runIdentTbl);

		runIdentTbls = this.runIdentTblHome.findByExample(runIdentTbl);

		if (runIdentTbls == null && runIdentTbls.isEmpty()) {
			this.error += "Run Name Not saved in database\n";
			return;
		}

		runIdentTbl = runIdentTbls.get(0);

		BaseRobotRules rules = null;

		try {
			rules = CrawlUtil.getWebsiteRules(Constants.USER_AGENT, this.targetUrl);
		} catch (Exception e) {
			e.printStackTrace();
		}

		if (rules != null && Boolean.TRUE.equals(rules.isAllowNone())) {
			this.error += "Webcrawling is not allowed on this site\n";
			return;
		}

		this.hasStarted = Boolean.TRUE;

		// Do web crawling here

		Queue<UrlProperty> urlProperties = new LinkedList<>();

		UrlProperty baseUrlProperty = new UrlProperty();

		baseUrlProperty.setName(this.targetUrl);

		urlProperties.add(baseUrlProperty);

		String baseDomain = null;
		try {
			baseDomain = CrawlUtil.getDomainName(this.targetUrl);
		} catch (URISyntaxException e1) {
			e1.printStackTrace();
			this.error += "Unable to fetch domain of the website\n";
			return;
		}

		while (Boolean.TRUE.equals(this.hasStarted) && Boolean.FALSE.equals(urlProperties.isEmpty())) {

			try {

				UrlProperty urlProperty = urlProperties.poll();

				// TODO: Make valuable request
				Connection connection = RequestResponseUtil.makeRequest(urlProperty);

				Document htmlDocument = connection.get();

				Document lastHtmlDocument = urlProperty.getHtmlDocument();
				Response lastResponse = urlProperty.getReponse();
				Request lastRequest = urlProperty.getRequest();

				Response response = connection.response();
				Request request = connection.request();

				// 200 is the HTTP OK status code
				if (response.statusCode() == 200) {

					if (Boolean.FALSE.equals(response.contentType().contains("text/html"))) {
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

					// TODO: Make logic to set transition number
					// requestResponseTbl.setPageTransitionIterationNumber(pageTransitionIterationNumber);
					requestResponseTbl.setRequestHeader(request.headers().toString());
					requestResponseTbl.setResponseBody(response.body());
					requestResponseTbl.setResponseHeader(response.headers().toString());
					requestResponseTbl.setRunIdentTbl(runIdentTbl);
					if (htmlDocument != null) {
						String title = "";

						Elements elems = htmlDocument.head().getElementsByTag("title");

						for (Element elem : elems) {
							title += elem.html();
						}

						// TODO: Make things fancy for title with numbers
						requestResponseTbl.setToPageTitle(title);
					}
					requestResponseTbl.setToPageUrl(urlProperty.getName());

					this.requestResponseTblHome.attachDirty(requestResponseTbl);

					Elements links = htmlDocument.select("a[href]");

					for (Element link : links) {

						String refectoredUrl = RequestResponseUtil.refectorUrl(link.attr("href"), baseDomain);

						Boolean isWithinDomain = CrawlUtil.isWithinDomain(this.targetUrl, RequestResponseUtil.refectorUrl(refectoredUrl, baseDomain));

						Boolean isAllowed = CrawlUtil.isAllowed(rules, this.targetUrl, refectoredUrl);

						if (Boolean.TRUE.equals(isWithinDomain) && Boolean.TRUE.equals(isAllowed)) {

							UrlProperty newUrlProperty = new UrlProperty();

							newUrlProperty.setName(refectoredUrl);
							newUrlProperty.setReponse(response);
							newUrlProperty.setRequest(connection.request());
							newUrlProperty.setHtmlDocument(htmlDocument);

							urlProperties.add(newUrlProperty);
						}
					}

					// logger.info("**Visiting** Received web page at " + url);
				} else {
					System.err.println("Connection Error - status code : " + response.statusCode());
					// logger.error("**Failure** Web page not recieved at " +
					// url);
				}

			} catch(UnsupportedMimeTypeException e) {
				// No need to log
			}catch (IOException e) {
				e.printStackTrace();
			}
			
		}

		Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(this.startTime, Calendar.getInstance().getTime());
		
		this.runTime = hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":" + hoursMinutesSeconds.get("seconds");
		
		this.hasStarted = Boolean.FALSE;
	}

	public void stop() {
		if (Boolean.TRUE.equals(this.hasStarted)) {
			// Calculate time
			Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(this.startTime, Calendar.getInstance().getTime());

			this.runTime = hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":" + hoursMinutesSeconds.get("seconds");

		} else {
			this.pagesMapped = 0;
			this.runTime = "00:00:00";
		}

		this.hasStarted = Boolean.FALSE;
	}

	public void fetchUpdates() {

		if (Boolean.TRUE.equals(this.hasStarted)) {
			// Calculate time
			Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(this.startTime, Calendar.getInstance().getTime());

			this.runTime = hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":" + hoursMinutesSeconds.get("seconds");

		} else {
			this.pagesMapped = 0;
			this.runTime = "00:00:00";
		}

	}

}
