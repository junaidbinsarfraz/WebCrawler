package com.webcrawler.util;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;

public class FileUtil {
	
	public static Boolean isFileExists(String filePath) {
		File f = new File(filePath);
		
		return (f.exists() && !f.isDirectory());
	}
	
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
