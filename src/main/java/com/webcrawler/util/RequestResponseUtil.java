package com.webcrawler.util;

import java.util.Map;

import org.jsoup.Connection;
import org.jsoup.Connection.Response;
import org.jsoup.Jsoup;

import com.webcrawler.model.UrlProperty;

/**
 * The class RequestResponseUtil is use to make request and get response
 * 
 * @author Junaid
 */
public class RequestResponseUtil {

	public static Connection makeRequest(UrlProperty urlProperty) {

		Response lastResponse = urlProperty.getLastReponse();

		// Proxy
		Connection connection = Jsoup.connect(urlProperty.getName()).proxy("127.0.0.1", com.webcrawler.jmeter.util.Constants.PROXY_PORT);

		connection.userAgent(Constants.USER_AGENT);
		connection.timeout(Constants.TIME_OUT);

		if (lastResponse != null) {
			Map<String, String> cookies = lastResponse.cookies();

			connection.cookies(cookies);

			String cookieString = "";

			for (Map.Entry<String, String> cookie : cookies.entrySet()) {
				cookieString = cookie.getKey() + "=" + cookie.getValue() + "; ";
			}

			if (!cookieString.isEmpty()) {
				connection.request().header("Cookie", cookieString);
			}
		}

		/*
		 * response.charset(); response.contentType(); response.headers();
		 * response.method();
		 */

		return connection;
	}

	/**
	 * The method refectorUrl() is use to refector given url
	 * 
	 * @param url
	 *            to be refectored
	 * @param domain
	 *            domain name
	 * @return refectored url
	 */
	public static String refectorUrl(String url, String domain) {
		String newUrl = null;

		if (url != null && !url.isEmpty()) {

			if (url.startsWith(" ")) {
				url = url.replaceFirst(" ", "");
			}

			if (!url.startsWith("http")) {
				if (url.startsWith("//")) {
					newUrl = "https:" + url;
				} else if (url.startsWith("/")) {
					newUrl = "http://" + domain + url;
				} else if (domain.endsWith("/")) {
					newUrl = "http://" + domain + url;
				} else {
					newUrl = "http://" + domain + "/" + url;
				}
			} else {
				newUrl = url;
			}

			if (Util.isNotNullAndEmpty(newUrl)) {
				newUrl = Util.trim(newUrl);
				newUrl = newUrl.replaceAll(" ", "%20");
				newUrl = newUrl.replaceAll("\\^", "%");
			}
		}

		return newUrl;
	}

}
