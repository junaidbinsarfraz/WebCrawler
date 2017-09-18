package com.webcrawler.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The class CorrelationUtil is use as a utility for correlation run
 */
public class CorrelationUtil {

	/**
	 * The extractArgunemtNameValue() method is use to extract all the argument
	 * name-value pairs from the jmxTree
	 * 
	 * @param jmeterXmlTree
	 *            contains the argument name-value pairs
	 * @return argument name-value pairs
	 */
	public static Map<String, String> extractArgunemtNameValue(String jmeterXmlTree) {

		Map<String, String> keyValues = new HashMap<>();

		Pattern argumentNamePattern = Pattern.compile("Prop name=\"Argument.name\">(.*?)</stringProp>");
		Matcher argumentNameMatcher = argumentNamePattern.matcher(jmeterXmlTree);

		Pattern argumentValuePattern = Pattern.compile("Prop name=\"Argument.value\">(.*?)</stringProp>|Prop name=\"Argument.value\"/>");
		Matcher argumentValueMatcher = argumentValuePattern.matcher(jmeterXmlTree);

		while (argumentNameMatcher.find() && argumentValueMatcher.find()) {
			keyValues.put(argumentNameMatcher.group(1), argumentValueMatcher.group(1));
		}

		return keyValues;
	}

	/**
	 * The extractHeaders() method is use to extract header's key-value pairs
	 * 
	 * @param requestHeader
	 *            contains the header's key-value pairs
	 * @param ignoreKeys
	 *            header keys to be ignored
	 * @return header's key-value pairs
	 */
	public static Map<String, String> extractHeaders(String requestHeader, List<String> ignoreKeys) {

		// clean the string
		requestHeader = requestHeader.replace("{", "");
		requestHeader = requestHeader.replace("}", "");

		Map<String, String> keyValues = new HashMap<>();

		String[] pairs = concatWrongSplittedString(requestHeader.split(","));

		for (String pair : pairs) {
			String[] splittedPair = pair.split("=");

			try {
				if (splittedPair != null && !ignoreKeys.stream().anyMatch(t -> t.equalsIgnoreCase(splittedPair[0].trim()))) {
					keyValues.put(splittedPair[0].trim(), splittedPair.length > 1 ? splittedPair[1].trim() : "");
				}
			} catch (Exception e) {
				System.out.println();
			}
		}

		return keyValues;
	}

	/**
	 * The concatWrongSplittedString() method is use to concat wrong splitted
	 * string array
	 * 
	 * @param str
	 *            array to be re arranged
	 * @return corrected string array
	 */
	private static String[] concatWrongSplittedString(String[] str) {

		List<String> splittedStr = new ArrayList<>();

		for (String pair : str) {
			if (!pair.contains("=")) {
				String lastPair = splittedStr.get(splittedStr.size() - 1);
				lastPair += pair;
				splittedStr.set(splittedStr.size() - 1, lastPair);
			} else {
				splittedStr.add(pair);
			}
		}

		return splittedStr.toArray(new String[splittedStr.size()]);
	}

}
