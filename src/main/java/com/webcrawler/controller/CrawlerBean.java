package com.webcrawler.controller;

import java.util.Date;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;

import org.openqa.selenium.WebDriver;

import com.webcrawler.jmeter.handler.RecordingHandler;

@ManagedBean(name = "crawlerBean")
@SessionScoped
public class CrawlerBean extends CrawlerController {

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
	private Integer port;
	
	// For Backend only
	private WebDriver driver;
	private RecordingHandler recordingHandler = new RecordingHandler();

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

	public Integer getPort() {
		return port;
	}

	public void setPort(Integer port) {
		this.port = port;
	}
	
	public WebDriver getDriver() {
		return driver;
	}

	public void setDriver(WebDriver driver) {
		this.driver = driver;
	}

	public RecordingHandler getRecordingHandler() {
		return recordingHandler;
	}

	public void setRecordingHandler(RecordingHandler recordingHandler) {
		this.recordingHandler = recordingHandler;
	}

	public void clear() {
		this.getValidationErrors().clear();
		this.error = "";
		this.pagesMapped = 0;
		this.runTime = "00:00:00";
	}

}
