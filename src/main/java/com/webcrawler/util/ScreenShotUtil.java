package com.webcrawler.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

public class ScreenShotUtil {
	
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
