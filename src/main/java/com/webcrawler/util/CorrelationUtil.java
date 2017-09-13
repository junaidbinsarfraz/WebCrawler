package com.webcrawler.util;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CorrelationUtil {

	public static Map<String, String> extractArgunemtNameValue(String jmeterXmlTree) {
		
		Map<String, String> keyValues = new HashMap<>();
		
		Pattern argumentNamePattern = Pattern.compile("Prop name=\"Argument.name\">(.*?)</stringProp>");
		Matcher argumentNameMatcher = argumentNamePattern.matcher(jmeterXmlTree);
		
		Pattern argumentValuePattern = Pattern.compile("Prop name=\"Argument.value\">(.*?)</stringProp>");
		Matcher argumentValueMatcher = argumentValuePattern.matcher(jmeterXmlTree);
		
		while (argumentNameMatcher.find() && argumentValueMatcher.find()) {
		    keyValues.put(argumentNameMatcher.group(1), argumentValueMatcher.group(1));
		}
		
		return keyValues;
	}
	
	public static Map<String, String> extractHeaders(String requestHeader, List<String> ignoreKeys) {
		
		// clean the string
		requestHeader = requestHeader.replace("{", "");
		requestHeader = requestHeader.replace("}", "");
		
		Map<String, String> keyValues = new HashMap<>();
		
		String[] pairs = requestHeader.split(",");
		
		for(String pair : pairs) {
			String[] splittedPair = pair.split("=");
			
			if(splittedPair != null && !ignoreKeys.stream().anyMatch(t -> t.equalsIgnoreCase(splittedPair[0]))) {
				keyValues.put(splittedPair[0], splittedPair[1]);
			}
		}
		
		return keyValues;
	}
	
}
