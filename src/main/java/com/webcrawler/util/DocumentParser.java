package com.webcrawler.util;

import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class DocumentParser {

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
	
	public static Document convertStringToHtmlDoc(String docString) {
		
		Document doc = null;
		
		try {
			doc = Jsoup.parse(docString);
		} catch (Exception e) {
		}
		
		return doc;
	}
	
}
