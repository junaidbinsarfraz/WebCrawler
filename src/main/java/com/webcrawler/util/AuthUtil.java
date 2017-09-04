package com.webcrawler.util;

import java.util.HashMap;
import java.util.Map;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import com.webcrawler.model.AuthenticationForm;

public class AuthUtil {
	
	public static String loginUsername = "enquiries@loadmetrics.co.uk";
	public static String loginPassword = "#Unst0n3#";

	public static Boolean isLoginSuccessful(Document document, AuthenticationForm loginForm) {
		
		if(loginForm == null) {
			return Boolean.FALSE;
		}
		
		// Locate authentication form
		Object form = null;
		form = findAndFillForm(document);
		if(form != null) {
			return Boolean.FALSE;
		}
		
		// Locate logout button/link, if found then return true
		
		// Locate error message if found the return false
		
		
		return Boolean.TRUE;
	}
	
	public static Boolean isLoginLink(Element loginLink) {
		
		for(String loginKey : DataUtil.getLoginKeys()) {
			if(loginKey.equalsIgnoreCase(loginLink.attr("href") != null ? loginLink.attr("href") : "")
					|| loginKey.equalsIgnoreCase(loginLink.attr("title") != null ? loginLink.attr("title") : "")
					|| loginLink.html().toLowerCase().contains(loginKey.toLowerCase())
					|| (loginLink.attr("id") != null && loginLink.attr("id").toLowerCase().contains(loginKey.toLowerCase()))) {
				return Boolean.TRUE;
			}
		}
		
		return Boolean.FALSE;
	}
	
	public static Boolean isLogoutLink(Element loginLink) {
		
		for(String loginKey : DataUtil.getLogoutKeys()) {
			if(loginKey.equalsIgnoreCase(loginLink.attr("href") != null ? loginLink.attr("href") : "")
					|| loginKey.equalsIgnoreCase(loginLink.attr("title") != null ? loginLink.attr("title") : "")
					|| loginLink.html().toLowerCase().contains(loginKey.toLowerCase())) {
				return Boolean.TRUE;
			}
		}
		
		return Boolean.FALSE;
	}
	
	public static AuthenticationForm findAndFillForm(Document document) {
		
		AuthenticationForm loginForm = null;
		
		Elements forms = document.getElementsByTag("form");
		
		if(forms.size() > 0) {
			
			for(Element myForm : forms) {
				// Get username and password field to check if form is login form or not
				// Also check for button or link's text if its login or not because it can be register link
				
				for(String usernameKey : DataUtil.getUsernameKeys()) {
					Elements usernameElems = myForm.getElementsByAttributeValueContaining("name", usernameKey);
					
					if(usernameElems.size() > 0) {
						for(String passwordKey : DataUtil.getPasswordKeys()) {
							Elements passwordElems = myForm.getElementsByAttributeValueContaining("name", passwordKey);
							
							if(passwordElems.size() > 0) {
								// Check for login button or link
								
								Elements loginButton = myForm.getElementsByAttributeValue("type", "submit");
								
								for(String loginKey : DataUtil.getLoginKeys()) {
									
									if(loginButton.size() > 0) {
										if((loginButton.first().attr("title") != null && loginButton.first().attr("title").toLowerCase().contains(loginKey.toLowerCase())) 
												|| (loginButton.first().text() != null && loginButton.first().text().toLowerCase().contains(loginKey.toLowerCase()))
												|| (loginButton.first().html().toLowerCase().contains(loginKey.toLowerCase()))
												|| (loginButton.first().attr("value") != null && loginButton.first().attr("value").contains(loginKey.toLowerCase()))) {
											// Login button/link exists
											
											Elements hiddenElems = myForm.select("input[type=hidden]");
									        Map<String, String> nameValue = new HashMap<>();

									        for(Element elem : hiddenElems) {
									            nameValue.put(elem.attr("name"), elem.attr("value"));
									        }
									        
									        nameValue.put(passwordElems.first().attr("name"), loginPassword);
									        nameValue.put(usernameElems.first().attr("name"), loginUsername);
									        
									        if(loginButton.first().attr("name") != null && loginButton.first().attr("name") != "") {
									        	nameValue.put(loginButton.first().attr("name"), loginButton.first().attr("value"));
									        }
											
											loginForm = new AuthenticationForm();
											
											loginForm.setData(nameValue);
											loginForm.setForm(myForm);
											loginForm.setPasswordField(passwordElems.first());
											loginForm.setUsernameField(usernameElems.first());
											loginForm.setLoginBtnField(loginButton.first());
											
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

}
