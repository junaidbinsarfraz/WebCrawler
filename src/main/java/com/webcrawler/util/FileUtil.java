package com.webcrawler.util;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;

/**
 * The class FileUtil is use to manipulate file
 * 
 * @author Junaid
 */
public class FileUtil {
	
	/**
	 * The method isFileExists() is use to check if file exists
	 * 
	 * @param filePath
	 *            file path
	 * @return true if File Exists
	 */
	public static Boolean isFileExists(String filePath) {
		File f = new File(filePath);
		
		return (f.exists() && !f.isDirectory());
	}
	
	/**
	 * The method extractUsernamePassword() is use to extract Username and
	 * Password from the given file with filePath
	 * 
	 * @param filePath
	 *            file
	 * @return username and password
	 */
	public static Map<String, String> extractUsernamePassword(String filePath) {
		Map<String, String> usernamePassword = new HashMap<>();
		
		if(Boolean.FALSE.equals(isFileExists(filePath))) {
			return null;
		}
		
		File file = new File(filePath);
		
		try {
			
			List<String> lines = FileUtils.readLines(file);
			
			if(Util.isNullOrEmpty(lines)) {
				return null;
			}
			
			if(lines.size() > 1) {
				return null;
			}
			
			String[] splitedStr = lines.get(0).split("/");
			
			usernamePassword.put("username", splitedStr[0]);
			usernamePassword.put("password", splitedStr[1]);
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return usernamePassword;
	}
	
}
