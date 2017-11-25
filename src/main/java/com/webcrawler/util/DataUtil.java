package com.webcrawler.util;

import java.util.ArrayList;
import java.util.List;

import com.webcrawler.dao.AuthTbl;
import com.webcrawler.dao.AuthTblHome;
import com.webcrawler.dao.HeaderIgnoreTbl;
import com.webcrawler.dao.HeaderIgnoreTblHome;
import com.webcrawler.dao.KnownExtractorsTbl;
import com.webcrawler.dao.KnownExtractorsTblHome;
import com.webcrawler.dao.PageCategoryTbl;
import com.webcrawler.dao.PageCategoryTblHome;

public final class DataUtil {
	
	private static List<String> usernameKeys = new ArrayList<>();
	private static List<String> passwordKeys = new ArrayList<>();
	private static List<String> loginKeys = new ArrayList<>();
	private static List<String> logoutKeys = new ArrayList<>();
	private static List<String> ignoreHeaderKeys = new ArrayList<>();
	private static List<KnownExtractorsTbl> knownExtractors = new ArrayList<>();
	private static List<PageCategoryTbl> pageCategories = new ArrayList<>();
	
	static {
		loadData();
	}
	
	public static void loadData() {
		// Fetch values from dao
		AuthTblHome authTblHome = new AuthTblHome();
		
		List<AuthTbl> authTbls = authTblHome.getAll();
		
		for(AuthTbl authTbl : authTbls) {
			if(Util.isNotNullAndEmpty(authTbl.getAuthId())) {
				usernameKeys.add(authTbl.getAuthId());
			}
			if(Util.isNotNullAndEmpty(authTbl.getAuthPass())) {
				passwordKeys.add(authTbl.getAuthPass());
			}
			if(Util.isNotNullAndEmpty(authTbl.getAuthOppNav())) {
				loginKeys.add(authTbl.getAuthOppNav());
			}
			if(Util.isNotNullAndEmpty(authTbl.getAuthSignoutNav())) {
				logoutKeys.add(authTbl.getAuthSignoutNav());
			}
		}
		
		 HeaderIgnoreTblHome headerIgnoreTblHome = new HeaderIgnoreTblHome();
		 
		 List<HeaderIgnoreTbl> headerIgnoreTbls = headerIgnoreTblHome.getAll();
		 
		 for(HeaderIgnoreTbl headerIgnoreTbl : headerIgnoreTbls) {
			 ignoreHeaderKeys.add(headerIgnoreTbl.getIgnoreHeader());
		 }
		 
		 KnownExtractorsTblHome knownExtractorsTblHome = new KnownExtractorsTblHome();
		 
		 knownExtractors = knownExtractorsTblHome.getAll();
		 
		 PageCategoryTblHome PageCategoryTblHome = new PageCategoryTblHome();
		 
		 pageCategories = PageCategoryTblHome.getAll();
	}

	public static List<String> getUsernameKeys() {
		return usernameKeys;
	}

	public static List<String> getPasswordKeys() {
		return passwordKeys;
	}

	public static List<String> getLoginKeys() {
		return loginKeys;
	}

	public static List<String> getLogoutKeys() {
		return logoutKeys;
	}
	
	public static List<String> getIgnoreHeaderKeys() {
		return ignoreHeaderKeys;
	}

	public static List<KnownExtractorsTbl> getKnownExtractors() {
		return knownExtractors;
	}

	public static List<PageCategoryTbl> getPageCategories() {
		return pageCategories;
	}

}
