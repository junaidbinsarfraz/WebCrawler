package com.webcrawler.parser;

import java.util.ArrayList;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import com.webcrawler.common.util.Util;

/**
 * The class DocumentParser is use to Parse Document
 * 
 * @author Junaid
 */
public class DocumentParser {

	/**
	 * The method getHiddenInputName() is use to extract hidden input name from
	 * document
	 * 
	 * @param doc
	 *            to be parsed
	 * @return list of hidden fields name
	 */
	public static List<String> getHiddenInputName(Document doc) {
		
		List<String> hiddenInputNames = new ArrayList<>();
		
		try {
			
			if(Util.isNotNull(doc)) {
				Elements elems = doc.select("input[type=hidden]");
				
				for(Element elem : elems) {
					hiddenInputNames.add(elem.attr("name"));
				}
			}
			
		} catch(Exception e) {
			
		}
		
		return hiddenInputNames;
	}
	
	/**
	 * The method convertStringToHtmlDoc() is use to convert given docString to
	 * Html Document
	 * 
	 * @param docString
	 *            to be converted
	 * @return Html Document
	 */
	public static Document convertStringToHtmlDoc(String docString) {
		
		Document doc = null;
		
		try {
			doc = Jsoup.parse(docString);
		} catch (Exception e) {
		}
		
		return doc;
	}
	
}
