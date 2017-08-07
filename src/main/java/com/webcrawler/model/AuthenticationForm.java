package com.webcrawler.model;

import org.jsoup.nodes.Element;

public class AuthenticationForm {

	private String username;
	private String password;
	private Element form;
	private Element usernameField;
	private Element passwordField;

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
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

}
