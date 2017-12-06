package com.webcrawler.controller;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;

@ManagedBean(name = "correlationBean")
@ViewScoped
public class CorrelationBean extends CorrelationController {
	
	private String correlationRunName;
	private String correlationError;
	private String correlationStatus;

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
	
	public void clean() {
		this.getValidationErrors().clear();
		
		this.correlationError = "";
		this.correlationStatus = "";
	}
	
}
