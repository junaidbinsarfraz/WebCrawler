package com.webcrawler.controller;

import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;

import org.hibernate.exception.JDBCConnectionException;

import com.webcrawler.common.controller.AbstractController;
import com.webcrawler.common.util.DateUtil;
import com.webcrawler.common.util.Util;
import com.webcrawler.dao.RequestResponseTbl;
import com.webcrawler.dao.RunIdentTbl;

@ManagedBean(name = "duplicateRemovalController")
@RequestScoped
public class DuplicateRemovalController extends AbstractController {
	
	/**
	 * The method startRemoval() is use to start duplicate page removal
	 */
	public void startRemoval() {
		
		DuplicateRemovalBean duplicateRemovalBean = getDuplicateRemovalBean();
		
		// Init
		Integer percentageValue = 0;
		
		// Validate inputs
		try {
			percentageValue = Integer.parseInt(duplicateRemovalBean.getPercentage());
			
			if(percentageValue <= 0) {
				duplicateRemovalBean.setDuplicateError(duplicateRemovalBean.getDuplicateError() + "Percentage should be a greater then 0<br/>");
			} else if(percentageValue > 100) {
				duplicateRemovalBean.setDuplicateError(duplicateRemovalBean.getDuplicateError() + "Percentage should be a less then 100<br/>");
			}
		} catch (Exception e) {
			// error
			duplicateRemovalBean.setDuplicateError(duplicateRemovalBean.getDuplicateError() + "Percentage should be a number<br/>");
		}
		
		// Check if run Name exists
		RunIdentTbl runIdentTbl = new RunIdentTbl();

		runIdentTbl.setRunIdentifier(duplicateRemovalBean.getRemovalRunName());

		List<RunIdentTbl> runIdentTbls = null;

		try {
			runIdentTbls = getRunIdentTblHome().findByRunName(runIdentTbl.getRunIdentifier());
		} catch (JDBCConnectionException e) {
			duplicateRemovalBean.setDuplicateError("Unable to connect to database<br/>");
		}

		if (runIdentTbls == null || Boolean.TRUE.equals(runIdentTbls.isEmpty())) {
			duplicateRemovalBean.setDuplicateError(duplicateRemovalBean.getDuplicateError() + "Run Name not found<br/>");
			return;
		}
		
		if (Util.isNotNullAndEmpty(duplicateRemovalBean.getDuplicateError())) {
			return;
		}
		
		for(RunIdentTbl runIdentTbl1 : runIdentTbls) {
			runIdentTbl = runIdentTbl1;
			
			// Check if run Name percentage is greater or not
			if (Boolean.TRUE.equals(runIdentTbl.getCleansed()) && percentageValue <= runIdentTbl.getPercent()) {
				duplicateRemovalBean.setDuplicateError(duplicateRemovalBean.getDuplicateError() + "Already cleansed with " + runIdentTbl.getPercent() + "%<br/>");
				continue;
			}
			
			runIdentTbl.setPercent(percentageValue);
			
			getRunIdentTblHome().attachDirty(runIdentTbl);
			
			runIdentTbl = getRunIdentTblHome().findById(runIdentTbl.getId());
			
			// Start doing removals
			RequestResponseTbl dummyRequestResponseTbl = new RequestResponseTbl();
			
			dummyRequestResponseTbl.setRunIdentTbl(runIdentTbl);
			
			// Get all parsed pages
			List<RequestResponseTbl> requestResponseTbls = getRequestResponseTblHome().findByRunId(runIdentTbl.getId());
			
			// Start Time
			duplicateRemovalBean.setStartTimeRemoval(Calendar.getInstance().getTime());
			
			duplicateRemovalBean.setHasStartedRemoval(Boolean.TRUE);
			duplicateRemovalBean.setHasFinishedRemoval(Boolean.FALSE);
			
			// Then apply bout-force algorithm
			for (Iterator<RequestResponseTbl> iterator = requestResponseTbls.iterator(); iterator.hasNext();) {
				if(Boolean.TRUE.equals(duplicateRemovalBean.getHasFinishedRemoval())) {
					break;
				}
				
				RequestResponseTbl requestResponseTbl = iterator.next();
				
				inner: for (RequestResponseTbl innerRequestResponseTbl : requestResponseTbls) {
					
					if(Boolean.TRUE.equals(duplicateRemovalBean.getHasFinishedRemoval())) {
						break;
					}
	
					if (Boolean.FALSE.equals(requestResponseTbl.getId().equals(innerRequestResponseTbl.getId()))
							&& requestResponseTbl.getAuthenticated() != null && innerRequestResponseTbl.getAuthenticated() != null
							&& requestResponseTbl.getAuthenticated() - innerRequestResponseTbl.getAuthenticated() == 0) { // Both should have same authenticated value
	
						/*Integer distance = CrawlUtil.levenshteinDistance(requestResponseTbl.getResponseBody(), innerRequestResponseTbl.getResponseBody());
	
						Double percentage = 100 - (((double) distance)
								/ (Math.max(requestResponseTbl.getResponseBody().length(), innerRequestResponseTbl.getResponseBody().length()))) * 100;*/
						
						if(requestResponseTbl.getToPageTitle().equalsIgnoreCase(innerRequestResponseTbl.getToPageTitle()) || 
								requestResponseTbl.getToPageUrl().equalsIgnoreCase(innerRequestResponseTbl.getToPageUrl())) {
						
							Double percentage = 100 - (( (double)Math.max(requestResponseTbl.getResponseBody().length(), innerRequestResponseTbl.getResponseBody().length())
									- (double)Math.min(requestResponseTbl.getResponseBody().length(), innerRequestResponseTbl.getResponseBody().length()))
									/ ((requestResponseTbl.getResponseBody().length() + innerRequestResponseTbl.getResponseBody().length()) / 2)) * 100;
		
							if (percentage != null && percentage.intValue() > percentageValue) {
		
								// Remove from database
								getRequestResponseTblHome().delete(requestResponseTbl);
								iterator.remove();
								duplicateRemovalBean.setPagesMappedRemoval(duplicateRemovalBean.getPagesMappedRemoval() + 1);
								break inner;
							}
						}
					}
				}
			}
			
			/*// Check Completely cleansed
			if(Boolean.FALSE.equals(hasFinishedRemoval)) {
				
				runIdentTbl.setCleansed(Boolean.TRUE);
				
				this.runIdentTblHome.attachDirty(runIdentTbl);
				
			}*/
		}
		
		runIdentTbl.setCleansed(Boolean.FALSE.equals(duplicateRemovalBean.getHasFinishedRemoval()));
		getRunIdentTblHome().attachDirty(runIdentTbl);
		
		duplicateRemovalBean.setHasStartedRemoval(Boolean.FALSE);
		duplicateRemovalBean.setHasFinishedRemoval(Boolean.TRUE);
	}

	/**
	 * The method stopRemoval() is use to stop duplicate page remova;
	 */
	public void stopRemoval() {
		
		DuplicateRemovalBean duplicateRemovalBean = getDuplicateRemovalBean();
		
		if (Boolean.TRUE.equals(duplicateRemovalBean.getHasStartedRemoval())) {
			// Calculate time
			Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(duplicateRemovalBean.getStartTimeRemoval(),
					Calendar.getInstance().getTime());

			duplicateRemovalBean.setRunTimeRemoval(hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":"
					+ hoursMinutesSeconds.get("seconds"));

			/*
			 * RequestContext reqCtx = RequestContext.getCurrentInstance();
			 * reqCtx.execute("poll.stop();");
			 */
			
		} else {
			duplicateRemovalBean.setPagesMappedRemoval(0);
			duplicateRemovalBean.setRunTimeRemoval("00:00:00");
		}

		duplicateRemovalBean.setHasStartedRemoval(Boolean.FALSE);
		duplicateRemovalBean.setHasFinishedRemoval(Boolean.TRUE);
	}

	/**
	 * The method fetchUpdatesRemoval() is use to update the timing and pages removed count
	 */
	public void fetchUpdatesRemoval() {

		DuplicateRemovalBean duplicateRemovalBean = getDuplicateRemovalBean();
		
		if (Boolean.TRUE.equals(duplicateRemovalBean.getHasStartedRemoval())) {
			// Calculate time
			Map<String, Long> hoursMinutesSeconds = DateUtil.getHoursMinutesSecondsDifference(duplicateRemovalBean.getStartTimeRemoval(),
					Calendar.getInstance().getTime());

			duplicateRemovalBean.setRunTimeRemoval(hoursMinutesSeconds.get("hours") + ":" + hoursMinutesSeconds.get("minutes") + ":"
					+ hoursMinutesSeconds.get("seconds"));

		} else if (Boolean.FALSE.equals(duplicateRemovalBean.getHasFinishedRemoval())) {
			duplicateRemovalBean.setPagesMappedRemoval(0);
			duplicateRemovalBean.setRunTimeRemoval("00:00:00");
		}
	}
	
}
