package com.webcrawler.util;

public interface Constants {
	
	String USER_AGENT = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/13.0.782.112 Safari/535.1";
	Integer TIME_OUT = 30 * 1000;
	Double PERCENTAGE_MATCH_MIN_LIMIT = 20.0;
	Integer MAX_DEPTH = 3;
//	String FIREFOX_PATH = "D:\\Junaid\\No Need\\FirefoxPortable\\App\\Firefox64\\firefox.exe";
//	String GECKODRIVER_PATH = "D:\\Junaid\\geckodriver\\geckodriver.exe";
	
//	String FIREFOX_PATH = "D:\\FirefoxPortable\\FirefoxPortable.exe";
//	String GECKODRIVER_PATH = "D:\\geckodriver\\geckodriver.exe";
	
	String FIREFOX_PATH = "E:\\Softwares\\FirefoxPortable\\App\\Firefox64\\firefox.exe";
	String GECKODRIVER_PATH = "E:\\Softwares\\geckodriver\\geckodriver.exe";
	
	String USERNAME_NICKNAME = "${aUser}";
	String PASSWORD_NICKNAME = "${aPword}";
	
	String CORR_REGEX = "=(.+?),";
	String REQUEST_PARAM_CORR_REGEX = "&quot;(.+?)&quot;;";
	String CORR_REGEX_EXTENDED_ARG_VALUE_INDICATOR = "value=";
	
	String UNCATEGORIZE_TO_PAGE_CATEGORY_TEXT = "Uncategorized";
	
//	String JMETER_HOME = "D://Junaid//apache-jmeter-3.2";
//	String JMETER_HOME = "D://apache-jmeter-3.2";
	String JMETER_HOME = "E://Softwares//apache-jmeter-3.2";
	Integer JEMTER_PORT = 8888;
	
	Integer PERCENTAGE_VALUE = 99;
	Integer PAGES_CRAWL_BEFORE_AUTHENTICATION = 5;
	
	Integer NUMBER_OF_EXTENDED_ARG_VALUE_CHARCTERS = 50;
	
}
