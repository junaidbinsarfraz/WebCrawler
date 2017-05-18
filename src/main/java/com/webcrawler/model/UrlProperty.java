package com.webcrawler.model;

import org.jsoup.Connection.Request;
import org.jsoup.Connection.Response;
import org.jsoup.nodes.Document;

public class UrlProperty {

	private String name;
	private Request lastRequest;
	private Response lastReponse;
	private Document htmlDocument;
	private String lastTitle;
	private Integer lastUniqueTitleCount;
	private Integer uniqueTitleCount;
	private Boolean titleModified;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Request getLastRequest() {
		return lastRequest;
	}

	public void setLastRequest(Request lastRequest) {
		this.lastRequest = lastRequest;
	}

	public Response getLastReponse() {
		return lastReponse;
	}

	public void setLastReponse(Response lastReponse) {
		this.lastReponse = lastReponse;
	}

	public Document getHtmlDocument() {
		return htmlDocument;
	}

	public void setHtmlDocument(Document htmlDocument) {
		this.htmlDocument = htmlDocument;
	}

	public String getLastTitle() {
		return lastTitle;
	}

	public void setLastTitle(String lastTitle) {
		this.lastTitle = lastTitle;
	}

	public Integer getUniqueTitleCount() {
		return uniqueTitleCount;
	}

	public void setUniqueTitleCount(Integer uniqueTitleCount) {
		this.uniqueTitleCount = uniqueTitleCount;
	}

	public Integer getLastUniqueTitleCount() {
		return lastUniqueTitleCount;
	}

	public void setLastUniqueTitleCount(Integer lastUniqueTitleCount) {
		this.lastUniqueTitleCount = lastUniqueTitleCount;
	}

	public Boolean getTitleModified() {
		return titleModified;
	}

	public void setTitleModified(Boolean titleModified) {
		this.titleModified = titleModified;
	}

}
