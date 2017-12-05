package com.webcrawler.controller;

public class CorrelationBean extends CorrelationController {
	
	private String correlationRunName;
	private String correlationError;
	private String correlationStatus;
	
	public void clean() {
		
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
	
}
