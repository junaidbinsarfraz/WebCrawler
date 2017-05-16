package com.webcrawler.model;

import org.jsoup.Connection.Request;
import org.jsoup.Connection.Response;
import org.jsoup.nodes.Document;

public class UrlProperty {

	private String name;
	private Request request;
	private Response reponse;
	private Document htmlDocument;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Request getRequest() {
		return request;
	}

	public void setRequest(Request request) {
		this.request = request;
	}

	public Response getReponse() {
		return reponse;
	}

	public void setReponse(Response reponse) {
		this.reponse = reponse;
	}

	public Document getHtmlDocument() {
		return htmlDocument;
	}

	public void setHtmlDocument(Document htmlDocument) {
		this.htmlDocument = htmlDocument;
	}

}
