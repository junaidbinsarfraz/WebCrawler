package com.webcrawler.dao;
// Generated Sep 12, 2017 10:44:29 AM by Hibernate Tools 5.1.0.Alpha1

/**
 * HeaderIgnoreTbl generated by hbm2java
 */
public class HeaderIgnoreTbl implements java.io.Serializable {

	private Integer id;
	private String ignoreHeader;

	public HeaderIgnoreTbl() {
	}

	public HeaderIgnoreTbl(String ignoreHeader) {
		this.ignoreHeader = ignoreHeader;
	}

	public Integer getId() {
		return this.id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public String getIgnoreHeader() {
		return this.ignoreHeader;
	}

	public void setIgnoreHeader(String ignoreHeader) {
		this.ignoreHeader = ignoreHeader;
	}

}
