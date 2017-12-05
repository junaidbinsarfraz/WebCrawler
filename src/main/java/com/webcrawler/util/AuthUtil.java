package com.webcrawler.util;

import java.util.HashMap;
import java.util.Map;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import com.webcrawler.common.util.Util;
import com.webcrawler.model.AuthenticationForm;

/**
 * The class AuthUtil is use to provide authenticate utilities
 * 
 * @author Junaid
 */
public class AuthUtil {

	// Default username and password
	public static String loginUsername = "enquiries@loadmetrics.co.uk";
	public static String loginPassword = "#Unst0n3#";

	/**
	 * The method isLoginSuccessful() is use to check if login is successful
	 * 
	 * @param document
	 *            to be parsed
	 * @param loginForm
	 *            last login form used to login
	 * @return true is document has loginForm else false
	 */
	public static Boolean isLoginSuccessful(Document document, AuthenticationForm loginForm) {

		if (loginForm == null) {
			return Boolean.FALSE;
		}

		// Locate authentication form
		Object form = null;
		form = findAndFillForm(document);
		if (form != null) {
			return Boolean.FALSE;
		}

		// Locate logout button/link, if found then return true

		// Locate error message if found the return false

		return Boolean.TRUE;
	}

	/**
	 * The method isLoginLink() is use to check if loginLink Element is login
	 * link or not
	 * 
	 * @param loginLink
	 *            to be parsed
	 * @return true if is login link else false
	 */
	public static Boolean isLoginLink(Element loginLink) {

		for (String loginKey : DataUtil.getLoginKeys()) {
			if (loginKey.equalsIgnoreCase(loginLink.attr("href") != null ? loginLink.attr("href") : "")
					|| loginKey.equalsIgnoreCase(loginLink.attr("title") != null ? loginLink.attr("title") : "")
					|| loginLink.html().toLowerCase().contains(loginKey.toLowerCase())
					|| (loginLink.attr("id") != null && loginLink.attr("id").toLowerCase().contains(loginKey.toLowerCase()))) {
				return Boolean.TRUE;
			}
		}

		return Boolean.FALSE;
	}

	/**
	 * The method isLogoutLink() is use to check if logoutLink Element is logout
	 * link or not
	 * 
	 * @param logoutLink
	 *            logout link element
	 * @return true if logout link else false
	 */
	public static Boolean isLogoutLink(Element logoutLink) {

		for (String loginKey : DataUtil.getLogoutKeys()) {
			if (loginKey.equalsIgnoreCase(logoutLink.attr("href") != null ? logoutLink.attr("href") : "")
					|| loginKey.equalsIgnoreCase(logoutLink.attr("title") != null ? logoutLink.attr("title") : "")
					|| logoutLink.html().toLowerCase().contains(loginKey.toLowerCase())) {
				return Boolean.TRUE;
			}
		}

		return Boolean.FALSE;
	}

	/**
	 * The method findAndFillForm() is use to find the login form and fill out
	 * login form with username and password
	 * 
	 * @param document
	 *            to be parsed
	 * @return authentication form is login form found else null
	 */
	public static AuthenticationForm findAndFillForm(Document document) {

		AuthenticationForm loginForm = null;

		Elements forms = document.getElementsByTag("form");

		if (forms.size() > 0) {

			for (Element myForm : forms) {
				// Get username and password field to check if form is login
				// form or not
				// Also check for button or link's text if its login or not
				// because it can be register link

				Elements formElems = myForm.getElementsByAttribute("name");

				for (String usernameKey : DataUtil.getUsernameKeys()) {
					for (Element usernameFormElem : formElems) {
						String usernameNameAttr = usernameFormElem.attr("name");

						if (Util.isNotNullAndEmpty(usernameNameAttr) && usernameNameAttr.toLowerCase().contains(usernameKey.toLowerCase())) {
							// Username field found

							for (String passwordKey : DataUtil.getPasswordKeys()) {
								for (Element passwordFormElem : formElems) {
									String passwordNameAttr = passwordFormElem.attr("name");

									if (Util.isNotNullAndEmpty(passwordNameAttr)
											&& passwordNameAttr.toLowerCase().contains(passwordKey.toLowerCase())) {
										// Password field found

										// Check for login button or link

										Elements loginButton = myForm.getElementsByAttributeValue("type", "submit");

										for (String loginKey : DataUtil.getLoginKeys()) {

											if (loginButton.size() > 0) {
												if ((loginButton.first().attr("title") != null
														&& loginButton.first().attr("title").toLowerCase().contains(loginKey.toLowerCase()))
														|| (loginButton.first().text() != null
																&& loginButton.first().text().toLowerCase().contains(loginKey.toLowerCase()))
														|| (loginButton.first().html().toLowerCase().contains(loginKey.toLowerCase()))
														|| (loginButton.first().attr("value") != null
																&& loginButton.first().attr("value").toLowerCase().contains(loginKey.toLowerCase()))) {
													// Login button/link exists

													Elements hiddenElems = myForm.select("input[type=hidden]");
													Map<String, String> nameValue = new HashMap<>();

													for (Element elem : hiddenElems) {
														nameValue.put(elem.attr("name"), elem.attr("value"));
													}

													nameValue.put(passwordFormElem.attr("name"), loginPassword);
													nameValue.put(usernameFormElem.attr("name"), loginUsername);

													if (loginButton.first().attr("name") != null && loginButton.first().attr("name") != "") {
														nameValue.put(loginButton.first().attr("name"), loginButton.first().attr("value"));
													}

													loginForm = new AuthenticationForm();

													loginForm.setData(nameValue);
													loginForm.setForm(myForm);
													loginForm.setPasswordField(passwordFormElem);
													loginForm.setUsernameField(usernameFormElem);
													loginForm.setLoginBtnField(loginButton.first());

													return loginForm;
												}
											}
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

}
