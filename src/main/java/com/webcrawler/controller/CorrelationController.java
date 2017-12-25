package com.webcrawler.controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.w3c.dom.Node;

import com.webcrawler.common.controller.AbstractController;
import com.webcrawler.common.util.Util;
import com.webcrawler.dao.CredsTbl;
import com.webcrawler.dao.HeaderCorrelationTbl;
import com.webcrawler.dao.JmeterTransControllerTbl;
import com.webcrawler.dao.PageCategoryTbl;
import com.webcrawler.dao.RequestCorrelationTbl;
import com.webcrawler.dao.RequestResponseTbl;
import com.webcrawler.dao.RunIdentTbl;
import com.webcrawler.parser.DocumentParser;
import com.webcrawler.parser.XmlParser;
import com.webcrawler.util.Constants;
import com.webcrawler.util.CorrelationUtil;
import com.webcrawler.util.DataUtil;

@ManagedBean(name = "correlationController")
@RequestScoped
public class CorrelationController extends AbstractController {
	
	/**
	 * The method startCorrelation() is use to start correlation for run name
	 */
	public void startCorrelation() {
		
		CorrelationBean correlationBean = getCorrelationBean();
		
		// Validate
		if(Util.isNullOrEmpty(correlationBean.getCorrelationRunName())) {
			correlationBean.setCorrelationError("Run Name is required");
			return;
		}
		
		// Check if already run exists
		RunIdentTbl runIdentTbl = new RunIdentTbl();
		
		runIdentTbl.setRunIdentifier(correlationBean.getCorrelationRunName());
		
		List<RunIdentTbl> runIdentTbls = getRunIdentTblHome().findByRunName(correlationBean.getCorrelationRunName());
		
		if(Util.isNullOrEmpty(runIdentTbls)) {
			correlationBean.setCorrelationError("Run Name doesnot exists");
			return;
		}
		
		runIdentTbl = runIdentTbls.get(0);
		
		List<HeaderCorrelationTbl> headerCorrelationTbls = getHeaderCorrelationTblHome().findByRunId(runIdentTbl.getId());
		
		if(Util.isNotNullAndEmpty(headerCorrelationTbls)) {
			correlationBean.setCorrelationError("Run Name is already correlated");
			return;
		}
		
		List<RequestCorrelationTbl> requestCorrelationTbls = getRequestCorrelationTblHome().findByRunId(runIdentTbl.getId());
		
		if(Util.isNotNullAndEmpty(requestCorrelationTbls)) {
			correlationBean.setCorrelationError("Run Name is already correlated");
			return;
		}
		
		Integer percentageValue = Constants.PERCENTAGE_VALUE;
		
		////////////////////////// Removal Starts
		
		for(RunIdentTbl runIdentTbl1 : runIdentTbls) {
			runIdentTbl = runIdentTbl1;
			
			runIdentTbl.setPercent(percentageValue);
			
			getRunIdentTblHome().attachDirty(runIdentTbl);
			
			runIdentTbl = getRunIdentTblHome().findById(runIdentTbl.getId());
			
			// Start doing removals
			RequestResponseTbl dummyRequestResponseTbl = new RequestResponseTbl();
			
			dummyRequestResponseTbl.setRunIdentTbl(runIdentTbl);
			
			// Get all parsed pages
			List<RequestResponseTbl> requestResponseTbls = getRequestResponseTblHome().findByRunId(runIdentTbl.getId());
			
			// Then apply bout-force algorithm
			for (Iterator<RequestResponseTbl> iterator = requestResponseTbls.iterator(); iterator.hasNext();) {
				
				RequestResponseTbl requestResponseTbl = iterator.next();
				
				inner: for (RequestResponseTbl innerRequestResponseTbl : requestResponseTbls) {
					
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
								break inner;
							}
						}
					}
				}
			}
		}
		
		runIdentTbl.setCleansed(Boolean.FALSE.equals(Boolean.TRUE));
		
		getRunIdentTblHome().attachDirty(runIdentTbl);
		
		///////////////////////////// Removal Ends
		
		runIdentTbl = (RunIdentTbl) getRunIdentTblHome().findByRunName(correlationBean.getCorrelationRunName()).get(0);
		
		Map<String, String> requestCorrelations = new HashMap<>();
		Map<String, String> requestExtendedCorrelations = new HashMap<>();
		Map<String, String> headerCorrelations = new HashMap<>();
		Map<String, String> responseHeaderCorrelations = new HashMap<>();
		Map<String, String> filteredHeaderCorrelations = new HashMap<>();
		Map<String, String> filteredRequestCorrelations = new HashMap<>();
		Map<String, RequestCorrelationTbl> filteredRequestCorrelationObjects = new HashMap<>();
		Map<String, String> filteredResponseHeaderCorrelations = new HashMap<>();
		
		List<RequestResponseTbl> requestResponseTbls = getRequestResponseTblHome().findByRunId(runIdentTbl.getId());
		
		// Get run name's request response table's request header
		for(RequestResponseTbl requestResponseTblTemp : requestResponseTbls) {
			if(requestResponseTblTemp.getJmeterTransControllerTbls() != null && !requestResponseTblTemp.getJmeterTransControllerTbls().isEmpty()) {
				
				Iterator it = requestResponseTblTemp.getJmeterTransControllerTbls().iterator();
				
				// Get run name's jmeter values 
				if(it.hasNext()) {
					JmeterTransControllerTbl jmeterTransControllerTbl = (JmeterTransControllerTbl) it.next();
					
					requestCorrelations.putAll(CorrelationUtil.extractArgunemtNameValue(jmeterTransControllerTbl.getTransContSec()));
					
					List<String> toBeRemovedKeys = new ArrayList<>();
					
					String responseBody = requestResponseTblTemp.getResponseBody();
					
					Map<String, String> hiddenInputs = DocumentParser.getHiddenInputs(responseBody, requestCorrelations.keySet().stream().collect(Collectors.toList()));
							
					for(Iterator<Map.Entry<String, String>> iterator = requestCorrelations.entrySet().iterator();
							iterator.hasNext();) {
						
						String key = iterator.next().getKey();
						
						try {
							
							String hiddenInput = hiddenInputs.get(key);
							
							if(Util.isNotNullAndEmpty(hiddenInput)) {
								
								Integer hiddenInputIndex = responseBody.indexOf(hiddenInput);
								
								while(hiddenInputIndex == -1) {
									hiddenInput = hiddenInput.substring(0, hiddenInput.length() -1);
									
									hiddenInputIndex = responseBody.indexOf(hiddenInput);
								}
								
								Integer indexOfKey = responseBody.indexOf(key, hiddenInputIndex);
								
								String extendedFoundArgValue = responseBody.substring(indexOfKey, indexOfKey + Constants.NUMBER_OF_EXTENDED_ARG_VALUE_CHARCTERS);
								
								if (Util.isNotNullAndEmpty(extendedFoundArgValue)
										&& extendedFoundArgValue.contains(Constants.CORR_REGEX_EXTENDED_ARG_VALUE_INDICATOR)) {
									
									requestExtendedCorrelations.put(key, extendedFoundArgValue);
								} else {
									toBeRemovedKeys.add(key);
								}
							}
						} catch (Exception e) {
							
						}
					}
					
					for(String key : toBeRemovedKeys) {
						requestCorrelations.remove(key);
					}
				}
			}
			
			headerCorrelations.putAll(CorrelationUtil.extractHeaders(requestResponseTblTemp.getRequestHeader(), DataUtil.getIgnoreHeaderKeys()));
			responseHeaderCorrelations.putAll(CorrelationUtil.extractHeaders(requestResponseTblTemp.getResponseHeader(), DataUtil.getIgnoreHeaderKeys()));
			
			// Set toPageCategory
			requestResponseTblTemp.setToPageCategory(Constants.UNCATEGORIZE_TO_PAGE_CATEGORY_TEXT);
			
			if(Util.isNotNullAndEmpty(requestResponseTblTemp.getToPageTitle())) {
				for(PageCategoryTbl pageCategoryTbl : DataUtil.getPageCategories()) {
					if(Util.isNotNullAndEmpty(pageCategoryTbl.getKeyword()) && 
							requestResponseTblTemp.getToPageTitle().toLowerCase().contains(pageCategoryTbl.getKeyword().toLowerCase())) {
						requestResponseTblTemp.setToPageCategory(pageCategoryTbl.getCategory());
						break;
					}
				}
			}
			
			getRequestResponseTblHome().attachDirty(requestResponseTblTemp);
		}
		
		Integer requestCorrelationVariable = 1;
		
		for(Map.Entry<String, String> requestCorrelation : requestCorrelations.entrySet()) {
			
			String requestExtendedCorrelationValue = requestExtendedCorrelations.get(requestCorrelation.getKey());
			
			if(Util.isNullOrEmpty(requestExtendedCorrelationValue)) {
				continue;
			}
			
			// Db operation
			RequestCorrelationTbl requestCorrelationTblTemp = new RequestCorrelationTbl();
			
			requestCorrelationTblTemp.setRunIdentTbl(runIdentTbl);
			requestCorrelationTblTemp.setFoundArgName(requestCorrelation.getKey());
			requestCorrelationTblTemp.setFoundArgValue(requestCorrelation.getValue());
			requestCorrelationTblTemp.setVariable("${cID" + String.format("%03d", requestCorrelationVariable++) + "}");
//			requestCorrelationTblTemp.setCorrRegex(requestCorrelationTblTemp.getFoundArgName() + Constants.REQUEST_PARAM_CORR_REGEX);
			requestCorrelationTblTemp.setFoundArgValueExtended(requestExtendedCorrelationValue);
			
			String separatingString = requestExtendedCorrelationValue.substring(requestExtendedCorrelationValue.indexOf(" "), 
					requestExtendedCorrelationValue.indexOf(Constants.CORR_REGEX_EXTENDED_ARG_VALUE_INDICATOR));
			
			requestCorrelationTblTemp.setCorrRegex(requestCorrelationTblTemp.getFoundArgName() + separatingString 
					+  Constants.CORR_REGEX_EXTENDED_ARG_VALUE_INDICATOR + Constants.REQUEST_PARAM_CORR_REGEX);
			
			filteredRequestCorrelations.put(requestCorrelationTblTemp.getFoundArgName(), requestCorrelationTblTemp.getVariable());
			filteredRequestCorrelationObjects.put(requestCorrelationTblTemp.getFoundArgName(), requestCorrelationTblTemp);
			
			// Put in database
			getRequestCorrelationTblHome().attachDirty(requestCorrelationTblTemp);
		}
		
		Integer headerCorrelationVariable = 1;
		
		for(Map.Entry<String, String> headerCorrelation : headerCorrelations.entrySet()) {
			
			// Remove duplicate
			if(!requestCorrelations.containsKey(headerCorrelation.getKey())) {
				
				HeaderCorrelationTbl headerCorrelationTblTemp = new HeaderCorrelationTbl();
				
				headerCorrelationTblTemp.setFoundHeaderName(headerCorrelation.getKey());
				headerCorrelationTblTemp.setFoundHeaderValue(headerCorrelation.getValue());
				headerCorrelationTblTemp.setRunIdentTbl(runIdentTbl);
				headerCorrelationTblTemp.setVariable("${hID" + String.format("%03d", headerCorrelationVariable++) + "}");
				headerCorrelationTblTemp.setCorrRegex(headerCorrelation.getKey() + Constants.CORR_REGEX);
				
				filteredHeaderCorrelations.put(headerCorrelation.getKey(), headerCorrelationTblTemp.getVariable());
				
				if(responseHeaderCorrelations.containsKey(headerCorrelationTblTemp.getFoundHeaderName())) {
					filteredResponseHeaderCorrelations.put(headerCorrelationTblTemp.getFoundHeaderName(), headerCorrelationTblTemp.getVariable());
				}
				
				// Put in database
				getHeaderCorrelationTblHome().attachDirty(headerCorrelationTblTemp);
			}
		}
		
		for(RequestResponseTbl requestResponseTblTemp : requestResponseTbls) {
			if(requestResponseTblTemp.getJmeterTransControllerTbls() != null && !requestResponseTblTemp.getJmeterTransControllerTbls().isEmpty()) {
				Iterator it = requestResponseTblTemp.getJmeterTransControllerTbls().iterator();
				
				// Get run name's jmeter values 
				if(it.hasNext()) {
					
					JmeterTransControllerTbl jmeterTransControllerTbl = (JmeterTransControllerTbl) it.next();
					
					List<Node> regexExtractors = new ArrayList<>();
					
					if((requestResponseTblTemp.getAuthenticated() == 1)) {
						
						Map<String, String> tempResponseHeaders = CorrelationUtil.extractHeaders(requestResponseTblTemp.getResponseHeader(), DataUtil.getIgnoreHeaderKeys());
						Map<String, String> finalResponseHeaders = new HashMap<>();
						
						for(Map.Entry<String, String> responseHeader : filteredResponseHeaderCorrelations.entrySet()) {
							if(tempResponseHeaders.containsKey(responseHeader.getKey())) {
								finalResponseHeaders.put(responseHeader.getKey() + Constants.CORR_REGEX, responseHeader.getValue());
							}
						}
						
						regexExtractors.addAll(XmlParser.createRegexExtractors(finalResponseHeaders, Boolean.TRUE));
						
						// update jmx value with header Correlation values
						jmeterTransControllerTbl.setTransContSec(XmlParser.parseRequestHeaderXmlAndUpdateValues(jmeterTransControllerTbl.getTransContSec(), 
								Util.isNullOrEmpty(requestResponseTblTemp.getRequestParameters()) ? filteredHeaderCorrelations : new HashMap<String, String>()));
						
						if(Util.isNotNullAndEmpty(requestResponseTblTemp.getRequestParameters())) {
							// TODO: update jmx value with request Correlation values
							List<CredsTbl> credsTbls = getCredsTblHome().findByRunId(runIdentTbl.getId());
							
							// update jmx parameter values with request correlation values
							if(Util.isNotNullAndEmpty(credsTbls)) {
								jmeterTransControllerTbl.setTransContSec(XmlParser.parseRequestArgumentXmlAndUpdateValues(jmeterTransControllerTbl.getTransContSec(), filteredRequestCorrelations, credsTbls.get(0).getUsername(), credsTbls.get(0).getPassword()));
							}
						}
					}
					
					// IT7R5
					String docString = requestResponseTblTemp.getResponseBody();
					
					try {
						Document doc = Jsoup.parse(docString);
						
						List<String> hiddenInputNames = DocumentParser.getHiddenInputName(doc);
						Map<String, String> foundFilteredRequestCorrelations = new HashMap<>();
						
						for(String hiddenInputName : hiddenInputNames) {
							if(Util.isNotNullAndEmpty(hiddenInputName) && filteredRequestCorrelationObjects.containsKey(hiddenInputName)) {
								RequestCorrelationTbl foundRequestCorrelationTbl = filteredRequestCorrelationObjects.get(hiddenInputName);
								foundFilteredRequestCorrelations.put(foundRequestCorrelationTbl.getCorrRegex(), foundRequestCorrelationTbl.getVariable());
							}
						}
						
						regexExtractors.addAll(XmlParser.createRegexExtractors(foundFilteredRequestCorrelations, Boolean.FALSE));
						
						jmeterTransControllerTbl.setTransContSec(XmlParser.appendRegexExtractorsToXml(jmeterTransControllerTbl.getTransContSec(), regexExtractors));
						
					} catch (Exception e) {
					}
					
					getJmeterTransControllerTblHome().attachDirty(jmeterTransControllerTbl);
				}
			}
		}
		
		correlationBean.setCorrelationStatus("Completed");
	}
}
