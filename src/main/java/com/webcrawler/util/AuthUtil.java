package com.webcrawler.util;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import com.webcrawler.model.AuthenticationForm;

public class AuthUtil {

	public static AuthenticationForm findForm(Document document, List<String> usernameKeys, List<String> passwordKeys, List<String> loginKeys) {

		AuthenticationForm loginForm = null;

		Elements forms = document.getElementsByTag("form");

		if (forms.size() > 0) {

			for (Element myForm : forms) {
				/*
				 * Get username and password field to check if form is login
				 * form or not Also check for button or link's text if its login
				 * or not because it can be register link
				 */
				for (String usernameKey : usernameKeys) {
					Elements usernameElems = myForm.getElementsByAttributeValue("name", usernameKey);

					if (usernameElems.size() > 0) {
						for (String passwordKey : passwordKeys) {
							Elements passwordElems = myForm.getElementsByAttributeValue("name", passwordKey);

							if (passwordElems.size() > 0) {
								// Check for login button or link

								Elements loginButton = myForm.getElementsByAttributeValue("type", "submit");

								for (String loginKey : loginKeys) {

									if (loginButton.size() > 0) {
										if (loginKey.equalsIgnoreCase(loginButton.attr("title")) || loginKey.equalsIgnoreCase(loginButton.text())
												|| loginButton.html().contains(loginKey) || loginKey.equalsIgnoreCase(loginButton.attr("value"))) {
											// Login button/link exists

											loginForm = new AuthenticationForm();

											loginForm.setForm(myForm);
											loginForm.setPassword(passwordKey);
											loginForm.setPasswordField(passwordElems.first());
											loginForm.setUsername(usernameKey);
											loginForm.setUsernameField(usernameElems.first());

											break;
										}
									}
								}
							}
						}
					}
				}
			}
		}

		return loginForm;
	}

	public static Map<String, String> getHiddenFields(Document document) {
		Map<String, String> hiddenFields = new HashMap<String, String>(0);

		// TODO: Get all hidden fields

		return hiddenFields;
	}
	
	public static Boolean isLoginSuccessful() {
		
		
		
		return Boolean.FALSE;
	}

}
