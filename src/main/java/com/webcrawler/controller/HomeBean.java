package com.webcrawler.controller;

import java.io.IOException;
import java.io.Serializable;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;

import org.jsoup.Connection;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

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
@ViewScoped
public class HomeBean implements Serializable {

	private String error;
	private String targetUrl;
	private String iterationPerPage;
	private String runName;
	private Boolean downloadImages;
	private String runTime;
	private Integer pagesMapped;
	private Boolean hasStarted;
	private Date startTime;

	private RunIdentTblHome runIdentTblHome = new RunIdentTblHome();

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

	public void start() {

		this.error = "";

		Integer iterations = null;

		// Validate inputs
		try {
			iterations = Integer.parseInt(this.iterationPerPage);
		} catch (Exception e) {
			this.error += "Iterations Per Page Transition must be a number<br/>";
		}

		if (iterations != null) {

			if (iterations > 100) {
				this.error += "Iterations Per Page Transition must be less then 100<br/>";
			} else if (iterations < 1) {
				this.error += "Iterations Per Page Transition must be greater then 0<br/>";
			}

		}

		// Get Run Name entry
		RunIdentTbl runIdentTbl = new RunIdentTbl();

		runIdentTbl.setRunIdentifier(this.runName);

		List<RunIdentTbl> runIdentTbls = this.runIdentTblHome.findByExample(runIdentTbl);

		if (runIdentTbls != null && Boolean.FALSE.equals(runIdentTbls.isEmpty())) {
			this.error += "Run Name already exists<br/>";
		}

		if (Util.isNotNullAndEmpty(this.error)) {
			return;
		}

		// Start Time
		this.startTime = Calendar.getInstance().getTime();

		this.runIdentTblHome.attachClean(runIdentTbl);

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

		this.hasStarted = Boolean.TRUE;

		// Do web crawling here

		Queue<UrlProperty> urlProperties = new LinkedList<>();

		UrlProperty baseUrlProperty = new UrlProperty();

		baseUrlProperty.setName(this.targetUrl);

		urlProperties.add(baseUrlProperty);

		while (Boolean.TRUE.equals(urlProperties.isEmpty())) {

			try {
				
				UrlProperty urlProperty = urlProperties.poll();

//				Connection connection = Jsoup.connect(this.targetUrl).userAgent(Constants.USER_AGENT).timeout(20 * 1000);

				// TODO: Make valuable request
				Connection connection = RequestResponseUtil.makeRequest(urlProperty);

				Document htmlDocument = connection.get();

				// 200 is the HTTP OK status code
				if (connection.response().statusCode() == 200) {
					UrlProperty newUrlProperty = new UrlProperty();

					// TODO: fetch all links and put them in the queue
					
					// logger.info("**Visiting** Received web page at " + url);
				} else {
					// logger.error("**Failure** Web page not recieved at " +
					// url);
				}
				if (Boolean.FALSE.equals(connection.response().contentType().contains("text/html"))) {
					// logger.error("**Failure** Retrieved something other than
					// HTML");
				}

			} catch (IOException e) {
				e.printStackTrace();
			}

		}

	}

	public void stop() {

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
