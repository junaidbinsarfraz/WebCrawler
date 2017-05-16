package com.webcrawler.util;

import org.jsoup.Connection;
import org.jsoup.Connection.Response;
import org.jsoup.Jsoup;

import com.webcrawler.model.UrlProperty;

public class RequestResponseUtil {

	public static Connection makeRequest(UrlProperty urlProperty) {

		// TODO: Make connection
		Response response = urlProperty.getReponse();
		
		Connection connection = Jsoup.connect(urlProperty.getName());
		
		connection.userAgent(Constants.USER_AGENT);
		connection.timeout(Constants.TIME_OUT);
		connection.cookies(response.cookies());
		
		/*response.charset();
		response.contentType();
		response.headers();
		response.method();*/
				
		return null;

	}

	public static String refectorUrl(String url, String domain) {
		String newUrl = null;

		if (url != null && !url.isEmpty()) {

			if (!url.startsWith("http")) {
				if (url.startsWith("//")) {
					newUrl = "https:" + url;
				} else if (url.startsWith("/")) {
					newUrl = "https://" + domain + url;
				}
			} else {
				newUrl = url;
			}

		}

		return newUrl;
	}

}
