package com.webcrawler.controller;

import java.util.Date;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;

@ManagedBean(name = "duplicateRemovalBean")
@ViewScoped
public class DuplicateRemovalBean extends DuplicateRemovalController {

	private String duplicateError;
	private String removalRunName;
	private String percentage;
	private String runTimeRemoval = "00:00:00";
	private Integer pagesMappedRemoval = 0;
	private Boolean hasStartedRemoval;
	private Boolean hasFinishedRemoval;
	private Date startTimeRemoval;

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
	
	public void clear() {
		this.getValidationErrors().clear();
		
		this.pagesMappedRemoval = 0;
		this.runTimeRemoval = "00:00:00";
		this.duplicateError = "";
	}

}
