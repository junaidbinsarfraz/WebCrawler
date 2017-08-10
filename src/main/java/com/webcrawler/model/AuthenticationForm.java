package com.webcrawler.model;

import java.util.HashMap;
import java.util.Map;

import org.jsoup.nodes.Element;

public class AuthenticationForm {

	private Map<String, String> data = new HashMap<>();
	private Element form;
	private Element usernameField;
	private Element passwordField;
	private Element loginBtnField;

	public Map<String, String> getData() {
		return data;
	}
	
	public void setData(Map<String, String> data) {
		this.data = data;
	}
	
	public Element getForm() {
		return form;
	}

	public void setForm(Element form) {
		this.form = form;
	}

	public Element getUsernameField() {
		return usernameField;
	}

	public void setUsernameField(Element usernameField) {
		this.usernameField = usernameField;
	}

	public Element getPasswordField() {
		return passwordField;
	}

	public void setPasswordField(Element passwordField) {
		this.passwordField = passwordField;
	}

	public Element getLoginBtnField() {
		return loginBtnField;
	}

	public void setLoginBtnField(Element loginBtnField) {
		this.loginBtnField = loginBtnField;
	}

}
