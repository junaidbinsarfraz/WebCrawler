package com.webcrawler.util;

import java.util.ArrayList;
import java.util.List;

import com.webcrawler.common.util.Util;
import com.webcrawler.dao.AuthTbl;
import com.webcrawler.dao.AuthTblHome;
import com.webcrawler.dao.HeaderIgnoreTbl;
import com.webcrawler.dao.HeaderIgnoreTblHome;
import com.webcrawler.dao.PageCategoryTbl;
import com.webcrawler.dao.PageCategoryTblHome;

/**
 * The class DataUtil contains data that will be initialize only once at the
 * start and hold data till end
 * 
 * @author Junaid
 */
public final class DataUtil {
	
	private static List<String> usernameKeys = new ArrayList<>();
	private static List<String> passwordKeys = new ArrayList<>();
	private static List<String> loginKeys = new ArrayList<>();
	private static List<String> logoutKeys = new ArrayList<>();
	private static List<String> ignoreHeaderKeys = new ArrayList<>();
	private static List<PageCategoryTbl> pageCategories = new ArrayList<>();
	
	static {
		loadData();
	}
	
	/**
	 * The method loadData() is use to load Data from the database at the start
	 */
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

	public static List<PageCategoryTbl> getPageCategories() {
		return pageCategories;
	}

}
