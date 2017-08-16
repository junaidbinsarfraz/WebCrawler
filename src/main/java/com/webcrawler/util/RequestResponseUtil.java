package com.webcrawler.util;

import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.URL;
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

	public static Connection makeRequest(UrlProperty urlProperty, Integer port, Boolean forLogin, Map<String, String> authCookies) {

		Response lastResponse = urlProperty.getLastReponse();

		String url = urlProperty.getName();
		
		if(Boolean.TRUE.equals(forLogin)) {
			url = CrawlUtil.beautifyActionUrl(urlProperty.getAuthForm().getForm().attr("action"), url);
		}
		
		// Connection making and proxy setting
		Connection connection = Jsoup.connect(url).proxy("127.0.0.1", port);

		connection.userAgent(Constants.USER_AGENT);
		connection.timeout(Constants.TIME_OUT);
		connection.validateTLSCertificates(Boolean.FALSE);

		if (lastResponse != null) {
			
			Map<String, String> cookies = null;//lastResponse.cookies();
			
			if(authCookies != null) {
				cookies = authCookies;
				cookies.putAll(lastResponse.cookies());
			} else {
				cookies = lastResponse.cookies();
			}

			connection.cookies(cookies);

			String cookieString = "";

			for (Map.Entry<String, String> cookie : cookies.entrySet()) {
				cookieString = cookie.getKey() + "=" + cookie.getValue() + "; ";
			}

			if (!cookieString.isEmpty()) {
				connection.request().header("Cookie", cookieString);
			}
		}
		
		if(forLogin) {
			connection.header("Referer", urlProperty.getName());
			connection.data(urlProperty.getAuthForm().getData());
		}

		return connection;
	}

	public static HttpURLConnection makeProxyRequest(UrlProperty urlProperty) {
		Response lastResponse = urlProperty.getLastReponse();

		HttpURLConnection conn = null;

		try {
			URL url = new URL(urlProperty.getName());
			Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress("127.0.0.1", 8080));
			conn = (HttpURLConnection) url.openConnection(proxy);

			conn.usingProxy();

			conn.setRequestMethod("GET");
			// conn.setRequestProperty("Host", "www.entea.fr");
			conn.setRequestProperty("User-Agent", Constants.USER_AGENT);
			conn.setRequestProperty("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8");
			conn.setRequestProperty("Accept-Language", "fr-FR,fr;q=0.8,en-US;q=0.6,en;q=0.4");
			// conn.setRequestProperty("Content-Type",
			// "application/x-www-form-urlencoded");

			if (lastResponse != null) {
				Map<String, String> cookies = lastResponse.cookies();

				String cookieString = "";

				for (Map.Entry<String, String> cookie : cookies.entrySet()) {
					cookieString = cookie.getKey() + "=" + cookie.getValue() + "; ";
				}

				if (!cookieString.isEmpty()) {
					conn.addRequestProperty("Set-Cookie", cookieString);
					conn.addRequestProperty("Cookie", cookieString);
				}
			}

			conn.setRequestProperty("Connection", "keep-alive");
			// conn.setRequestProperty("Referer",
			// "https://www.entea.fr/CookieAuth.dll?GetLogon?curl=Z2F&reason=0&formdir=7");
			conn.setRequestProperty("Accept-Encoding", "gzip,deflate");
			// conn.setRequestProperty("Content-Type",
			// "application/x-www-form-urlencoded");
			// conn.setRequestProperty("Content-Length",
			// Integer.toString(postParams.length()));
			conn.setDoOutput(true);
			conn.setDoInput(true);

		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return conn;
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
