package com.webcrawler.jmeter.util;

import java.io.ByteArrayInputStream;
import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import org.apache.commons.io.FileUtils;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

public class XmlParser {

	public static String parseXmlWithGivenXPathExp(File xmlFile, String xpathExp) {
		
		String parsedXml = "";
		
		try {
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			
			ByteArrayInputStream byteArray = new ByteArrayInputStream(FileUtils.readFileToByteArray(xmlFile));
			
			Document doc = builder.parse(byteArray);
			
			XPath xpath = XPathFactory.newInstance().newXPath();
			XPathExpression expr = xpath.compile(xpathExp);
			Object exprResult = expr.evaluate(doc, XPathConstants.NODESET);
			NodeList nodeList = (NodeList) exprResult;
			
			System.out.println(nodeList.getLength());
			System.out.println(nodeList.toString());
			
		} catch (Exception e){
			return null;
		}
		
		return parsedXml;
	}
	
}
