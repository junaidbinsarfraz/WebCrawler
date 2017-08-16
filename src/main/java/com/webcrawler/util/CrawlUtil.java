package com.webcrawler.util;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.entity.BufferedHttpEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HttpContext;
import org.apache.http.util.EntityUtils;

import crawlercommons.robots.BaseRobotRules;
import crawlercommons.robots.SimpleRobotRules;
import crawlercommons.robots.SimpleRobotRules.RobotRulesMode;
import crawlercommons.robots.SimpleRobotRulesParser;

/**
 * The class CrawlUtil is use as a util class for crawling
 * 
 * @author Junaid
 */
public class CrawlUtil {

	/**
	 * The method getWebsiteRules() is use to extract all the website's crawl
	 * rules
	 * 
	 * @param userAgent
	 *            to be used to access website
	 * @param url
	 *            website url
	 * @return All robot rules
	 * @throws Exception
	 */
	public static BaseRobotRules getWebsiteRules(String userAgent, String url) throws Exception {

		URL urlObj = new URL(url);
		String hostId = urlObj.getProtocol() + "://" + urlObj.getHost() + (urlObj.getPort() > -1 ? ":" + urlObj.getPort() : "");
		BaseRobotRules rules = null;
		HttpGet httpget = new HttpGet(hostId + "/robots.txt");
		HttpContext context = new BasicHttpContext();
		HttpClient httpclient = HttpClientBuilder.create().build();
		HttpResponse response = httpclient.execute(httpget, context);

		if (response.getStatusLine() != null && response.getStatusLine().getStatusCode() == 404) {
			rules = new SimpleRobotRules(RobotRulesMode.ALLOW_ALL);
			// consume entity to deallocate connection
			EntityUtils.consume(response.getEntity());
		} else {
			BufferedHttpEntity entity = new BufferedHttpEntity(response.getEntity());
			SimpleRobotRulesParser robotParser = new SimpleRobotRulesParser();
			rules = robotParser.parseContent(hostId, IOUtils.toByteArray(entity.getContent()), "text/plain", userAgent);
		}

		return rules;
	}

	/**
	 * The method isAllowed() is use to check if website allow to crawl the give
	 * page or not
	 * 
	 * @param rules
	 *            website robot rules
	 * @param baseUrl
	 *            website url
	 * @param toUrl
	 *            to be checked
	 * @return true if toUrl is allowed else false
	 */
	public static Boolean isAllowed(BaseRobotRules rules, String baseUrl, String toUrl) {

		if (Boolean.TRUE.equals(isWithinDomain(baseUrl, toUrl))) {

			if (rules == null || rules.isAllowAll() || rules.isAllowed(toUrl)) {
				return Boolean.TRUE;
			}
		}

		return Boolean.FALSE;
	}

	/**
	 * The method isWithinDomain() is use to check if toUrl is within baseUrl's
	 * domain or not
	 * 
	 * @param baseUrl
	 *            website base url
	 * @param toUrl
	 *            to be checked
	 * @return true is toUrl is within domai
	 */
	public static Boolean isWithinDomain(String baseUrl, String toUrl) {

		try {
			if (Util.isNotNullAndEmpty(baseUrl) && Util.isNotNullAndEmpty(toUrl)
					&& (getDomainName(baseUrl).equals(getDomainName(toUrl)) || getDomainName(toUrl).contains(getDomainName(baseUrl)))) {
				return Boolean.TRUE;
			}
		} catch (URISyntaxException e) {
			// e.printStackTrace();
			// Donot need to log
//			System.out.println("Url syntax is inccorrect : " + toUrl);
		}

		return Boolean.FALSE;
	}

	/**
	 * The method getDomainName() is use to extract domain name from the url
	 * 
	 * @param url
	 *            given url
	 * @return domain
	 * @throws URISyntaxException
	 */
	public static String getDomainName(String url) throws URISyntaxException {
		URI uri = new URI(url);
		String domain = uri.getHost();
		return domain.startsWith("www.") ? domain.substring(4) : domain;
	}
	
	/**
	 * The method getFullDomainName() is use to extract full domain name from the url
	 * 
	 * @param url
	 *            given url
	 * @return domain
	 * @throws URISyntaxException
	 */
	public static String getFullDomainName(String url) throws URISyntaxException {
		URI uri = new URI(url);
		String domain = uri.getHost();
		return domain;
	}

	/**
	 * The method levenshteinDistance() is use to calculate the distance between
	 * two strings
	 * 
	 * @param lhs
	 *            first string
	 * @param rhs
	 *            secont sreing
	 * @return distance
	 */
	public static int levenshteinDistance(CharSequence lhs, CharSequence rhs) {
		int len0 = lhs.length() + 1;
		int len1 = rhs.length() + 1;

		// the array of distances
		int[] cost = new int[len0];
		int[] newcost = new int[len0];

		// initial cost of skipping prefix in String s0
		for (int i = 0; i < len0; i++)
			cost[i] = i;

		// dynamically computing the array of distances

		// transformation cost for each letter in s1
		for (int j = 1; j < len1; j++) {
			// initial cost of skipping prefix in String s1
			newcost[0] = j;

			// transformation cost for each letter in s0
			for (int i = 1; i < len0; i++) {
				// matching current letters in both strings
				int match = (lhs.charAt(i - 1) == rhs.charAt(j - 1)) ? 0 : 1;

				// computing cost for each transformation
				int cost_replace = cost[i - 1] + match;
				int cost_insert = cost[i] + 1;
				int cost_delete = newcost[i - 1] + 1;

				// keep minimum cost
				newcost[i] = Math.min(Math.min(cost_insert, cost_delete), cost_replace);
			}

			// swap cost/newcost arrays
			int[] swap = cost;
			cost = newcost;
			newcost = swap;
		}

		// the distance is the cost for transforming all letters in both strings
		return cost[len0 - 1];
	}

	/**
	 * The method beautifyActionUrl() is use to beautify action url of form
	 * 
	 * @param actionUrl
	 *            to be beautified
	 * @param loginUrl
	 *            referenced url
	 * @return beautified action url
	 */
	public static String beautifyActionUrl(String actionUrl, String loginUrl) {
		if (actionUrl.startsWith("http://") || actionUrl.startsWith("https://")) {
			return actionUrl;
		}

		String domainUrl = loginUrl;

		try {
			domainUrl = getDomainName(loginUrl);
			domainUrl = loginUrl.contains("www.") ? "www." + domainUrl : domainUrl;
			domainUrl = loginUrl.startsWith("https://") ? "https://" + domainUrl : "http://" + domainUrl;
		} catch (URISyntaxException e) {
			e.printStackTrace();
		}

		if (actionUrl.equals("/") || actionUrl.equals("./")) {
			return domainUrl.endsWith("/") ? domainUrl : domainUrl + "/";
		}

		return (domainUrl + actionUrl);
	}

}