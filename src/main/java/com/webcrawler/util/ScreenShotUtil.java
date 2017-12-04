package com.webcrawler.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.concurrent.TimeUnit;

import org.openqa.selenium.WebDriver;
import org.openqa.selenium.firefox.FirefoxBinary;
import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.firefox.FirefoxProfile;

/**
 * The class ScreenShotUtil is use to take screen shot
 * 
 * @author Junaid
 */
public class ScreenShotUtil {

	/**
	 * The method initFireFox() is use to init firefox
	 * 
	 * @return WebDriver
	 */
	@SuppressWarnings("deprecation")
	public static WebDriver initFireFox() {

		WebDriver driver = null;

		try {
			System.setProperty("webdriver.gecko.driver", Constants.GECKODRIVER_PATH);
			File pathToBinary = new File(Constants.FIREFOX_PATH);
			FirefoxBinary ffBinary = new FirefoxBinary(pathToBinary);
			FirefoxProfile firefoxProfile = new FirefoxProfile();
			/*
			 * firefoxProfile.setPreference("network.proxy.type", 1);
			 * firefoxProfile.setPreference("network.proxy.http", "127.0.0.1");
			 * firefoxProfile.setPreference("network.proxy.http_port", 8888);
			 */
			firefoxProfile.setPreference("toolkit.startup.max_resumed_crashes", "-1");
			driver = new FirefoxDriver(ffBinary, firefoxProfile);
			driver.manage().timeouts().pageLoadTimeout(new Long(1), TimeUnit.MINUTES);
			driver.manage().timeouts().implicitlyWait(new Long(1), TimeUnit.MINUTES);
			driver.manage().timeouts().setScriptTimeout(new Long(1), TimeUnit.MINUTES);
			
		} catch (Exception e) {
			System.out.println("Unable to initialize firefox driver\n" + e);
		}

		return driver;
	}

	/**
	 * The method killFirefox() is use to kill firefox
	 */
	public static void killFirefox() {
		Runtime rt = Runtime.getRuntime();

		try {
			rt.exec("taskkill /F /IM firefox.exe");
			while (processIsRunning("firefox.exe")) {
				Thread.sleep(100);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * The method processIsRunning() is use to check if firefox-screen-shot
	 * process is running
	 * 
	 * @param process
	 *            to be checked
	 * @return true if process is running
	 */
	private static boolean processIsRunning(String process) {
		boolean processIsRunning = false;
		String line;
		try {
			Process proc = Runtime.getRuntime().exec("wmic.exe");
			BufferedReader input = new BufferedReader(new InputStreamReader(proc.getInputStream()));
			OutputStreamWriter oStream = new OutputStreamWriter(proc.getOutputStream());
			oStream.write("process where name='" + process + "'");
			oStream.flush();
			oStream.close();
			while ((line = input.readLine()) != null) {
				if (line.toLowerCase().contains("caption")) {
					processIsRunning = true;
					break;
				}
			}
			input.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return processIsRunning;
	}

}
