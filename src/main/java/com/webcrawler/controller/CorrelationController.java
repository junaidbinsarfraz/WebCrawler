package com.webcrawler.controller;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

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
import com.webcrawler.dao.KnownExtractorsTbl;
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
		
		this.correlationError = "";
		this.correlationStatus = "";
		
		// Validate
		if(Util.isNullOrEmpty(this.correlationRunName)) {
			this.correlationError = "Run Name is required";
			return;
		}
		
		// Check if already run exists
		RunIdentTbl runIdentTbl = new RunIdentTbl();
		
		runIdentTbl.setRunIdentifier(this.correlationRunName);
		
		List<RunIdentTbl> runIdentTbls = this.runIdentTblHome.findByRunName(this.correlationRunName);
		
		if(Util.isNullOrEmpty(runIdentTbls)) {
			this.correlationError = "Run Name doesnot exists";
			return;
		}
		
		runIdentTbl = runIdentTbls.get(0);
		
		List<HeaderCorrelationTbl> headerCorrelationTbls = this.headerCorrelationTblHome.findByRunId(runIdentTbl.getId());
		
		if(Util.isNotNullAndEmpty(headerCorrelationTbls)) {
			this.correlationError = "Run Name is already correlated";
			return;
		}
		
		List<RequestCorrelationTbl> requestCorrelationTbls = this.requestCorrelationTblHome.findByRunId(runIdentTbl.getId());
		
		if(Util.isNotNullAndEmpty(requestCorrelationTbls)) {
			this.correlationError = "Run Name is already correlated";
			return;
		}
		
		Map<String, String> requestCorrelations = new HashMap<>();
		Map<String, String> headerCorrelations = new HashMap<>();
		Map<String, String> responseHeaderCorrelations = new HashMap<>();
		Map<String, String> filteredHeaderCorrelations = new HashMap<>();
		Map<String, String> filteredRequestCorrelations = new HashMap<>();
		Map<String, RequestCorrelationTbl> filteredRequestCorrelationObjects = new HashMap<>();
		Map<String, String> filteredResponseHeaderCorrelations = new HashMap<>();
		
		List<RequestResponseTbl> requestResponseTbls = this.requestResponseTblHome.findByRunId(runIdentTbl.getId());
		
		// Get run name's request response table's request header
		for(RequestResponseTbl requestResponseTblTemp : requestResponseTbls) {
			if(requestResponseTblTemp.getJmeterTransControllerTbls() != null && !requestResponseTblTemp.getJmeterTransControllerTbls().isEmpty()) {
				
				Iterator it = requestResponseTblTemp.getJmeterTransControllerTbls().iterator();
				
				// Get run name's jmeter values 
				if(it.hasNext()) {
					JmeterTransControllerTbl jmeterTransControllerTbl = (JmeterTransControllerTbl) it.next();
					
					requestCorrelations.putAll(CorrelationUtil.extractArgunemtNameValue(jmeterTransControllerTbl.getTransContSec()));
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
			
			this.requestResponseTblHome.attachDirty(requestResponseTblTemp);
			
		}
		
		Integer requestCorrelationVariable = 1;
		
		for(Map.Entry<String, String> requestCorrelation : requestCorrelations.entrySet()) {
			
			// Db operation
			RequestCorrelationTbl requestCorrelationTblTemp = new RequestCorrelationTbl();
			
			requestCorrelationTblTemp.setRunIdentTbl(runIdentTbl);
			requestCorrelationTblTemp.setFoundArgName(requestCorrelation.getKey());
			requestCorrelationTblTemp.setFoundArgValue(requestCorrelation.getValue());
			requestCorrelationTblTemp.setVariable("${cID" + String.format("%03d", requestCorrelationVariable++) + "}");
			requestCorrelationTblTemp.setCorrRegex(requestCorrelationTblTemp.getFoundArgName() + Constants.REQUEST_PARAM_CORR_REGEX);
			
			for(KnownExtractorsTbl knownExtractorsTbl : DataUtil.getKnownExtractors()) {
				if(Util.isNotNullAndEmpty(knownExtractorsTbl.getKnownValue()) 
						&& knownExtractorsTbl.getKnownValue().equalsIgnoreCase(requestCorrelationTblTemp.getFoundArgName())) {
					requestCorrelationTblTemp.setCorrRegex(knownExtractorsTbl.getExtractor());
				}
			}
			
			filteredRequestCorrelations.put(requestCorrelationTblTemp.getFoundArgName(), requestCorrelationTblTemp.getVariable());
			filteredRequestCorrelationObjects.put(requestCorrelationTblTemp.getFoundArgName(), requestCorrelationTblTemp);
			
			// Put in database
			this.requestCorrelationTblHome.attachDirty(requestCorrelationTblTemp);
		}
		
		Integer headerCorrelationVariable = 1;
		
		for(Map.Entry<String, String> headerCorrelation : headerCorrelations.entrySet()) {
			
			// Remove duplicate
			if(!requestCorrelations.containsKey(headerCorrelation.getKey())) {
				
//				filteredHeaderCorrelations.put(headerCorrelation.getKey(), headerCorrelation.getValue());
				
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
				this.headerCorrelationTblHome.attachDirty(headerCorrelationTblTemp);
			}
		}
		
//		List<Node> regexExtractors = XmlParser.createRegexExtractors(filteredHeaderCorrelations);
//		List<Node> regexExtractors = XmlParser.createRegexExtractors(filteredResponseHeaderCorrelations);
		
		for(RequestResponseTbl requestResponseTblTemp : requestResponseTbls) {
			if(requestResponseTblTemp.getJmeterTransControllerTbls() != null && !requestResponseTblTemp.getJmeterTransControllerTbls().isEmpty()) {
				Iterator it = requestResponseTblTemp.getJmeterTransControllerTbls().iterator();
				
				// Get run name's jmeter values 
				if(it.hasNext()) {
					
					JmeterTransControllerTbl jmeterTransControllerTbl = (JmeterTransControllerTbl) it.next();
					
					if((requestResponseTblTemp.getAuthenticated() == 1)) {
						
						Map<String, String> tempResponseHeaders = CorrelationUtil.extractHeaders(requestResponseTblTemp.getResponseHeader(), DataUtil.getIgnoreHeaderKeys());
						Map<String, String> finalResponseHeaders = new HashMap<>();
						
						for(Map.Entry<String, String> responseHeader : filteredResponseHeaderCorrelations.entrySet()) {
							if(tempResponseHeaders.containsKey(responseHeader.getKey())) {
								finalResponseHeaders.put(responseHeader.getKey() + Constants.CORR_REGEX, responseHeader.getValue());
							}
						}
						
						List<Node> regexExtractors = XmlParser.createRegexExtractors(finalResponseHeaders, Boolean.TRUE);
						
						// update jmx value with header Correlation values
						jmeterTransControllerTbl.setTransContSec(XmlParser.parseRequestHeaderXmlAndUpdateValues(jmeterTransControllerTbl.getTransContSec(), 
								Util.isNullOrEmpty(requestResponseTblTemp.getRequestParameters()) ? filteredHeaderCorrelations : new HashMap<String, String>(), regexExtractors));
						
						if(Util.isNotNullAndEmpty(requestResponseTblTemp.getRequestParameters())) {
							// TODO: update jmx value with request Correlation values
							List<CredsTbl> credsTbls = credsTblHome.findByRunId(runIdentTbl.getId());
							
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
						
						List<Node> regexExtractors = XmlParser.createRegexExtractors(foundFilteredRequestCorrelations, Boolean.FALSE);
						
						jmeterTransControllerTbl.setTransContSec(XmlParser.addRequestParametersAsRegexExtractors(jmeterTransControllerTbl.getTransContSec(), regexExtractors));
						
					} catch (Exception e) {
					}
					
					
					this.jmeterTransControllerTblHome.attachDirty(jmeterTransControllerTbl);
				
				}
			}
		}
		
		this.correlationStatus = "Completed";
		
	}
	
}
