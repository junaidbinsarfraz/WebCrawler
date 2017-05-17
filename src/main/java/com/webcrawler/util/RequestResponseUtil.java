package com.webcrawler.util;

import java.util.Map;

import org.jsoup.Connection;
import org.jsoup.Connection.Response;
import org.jsoup.Jsoup;

import com.webcrawler.model.UrlProperty;

public class RequestResponseUtil {

	public static Connection makeRequest(UrlProperty urlProperty) {

		Response lastResponse = urlProperty.getReponse();
		
		Connection connection = Jsoup.connect(urlProperty.getName());
		
		connection.userAgent(Constants.USER_AGENT);
		connection.timeout(Constants.TIME_OUT);
		
		if(lastResponse != null) {
			Map<String, String> cookies = lastResponse.cookies();
			
			connection.cookies(cookies);
			
			String cookieString = "";
			
			for (Map.Entry<String, String> cookie : cookies.entrySet()) {
				cookieString = cookie.getKey() + "=" + cookie.getValue() + "; ";
			}
			
			if(!cookieString.isEmpty()) {
				connection.request().header("Cookie", cookieString);
			}
		}
		
		/*response.charset();
		response.contentType();
		response.headers();
		response.method();*/
				
		return connection;

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
