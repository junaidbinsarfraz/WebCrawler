package com.webcrawler.jmeter.handler;

import java.io.File;
import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;

import org.apache.jmeter.control.LoopController;
import org.apache.jmeter.gui.tree.JMeterTreeModel;
import org.apache.jmeter.gui.tree.JMeterTreeNode;
import org.apache.jmeter.protocol.http.control.RecordingController;
import org.apache.jmeter.protocol.http.proxy.ProxyControl;
import org.apache.jmeter.save.SaveService;
import org.apache.jmeter.testelement.TestElement;
import org.apache.jmeter.testelement.TestPlan;
import org.apache.jmeter.threads.ThreadGroup;
import org.apache.jmeter.util.JMeterUtils;
import org.apache.jorphan.collections.HashTree;
import org.apache.jorphan.collections.ListedHashTree;

import com.webcrawler.jmeter.util.Constants;

public class RecordingHandler {

	private ProxyControl proxy = null;
	private JMeterTreeModel treeModel = null;

	@SuppressWarnings("deprecation")
	public void init() throws Exception {

		JMeterUtils.setJMeterHome(Constants.JMETER_HOME); // Or wherever you put
															// it.
		JMeterUtils.loadJMeterProperties(JMeterUtils.getJMeterBinDir() + "/jmeter.properties");
		JMeterUtils.initLocale();

		LoopController loopController = new LoopController();
		loopController.setEnabled(true);
		loopController.setLoops(1);

		RecordingController rc = new RecordingController();
		rc.setName("Recording Controller");

		TestPlan testPlan = new TestPlan();

		ThreadGroup threadGroup1 = new ThreadGroup();
		threadGroup1.setSamplerController(loopController);
		threadGroup1.addTestElement(rc); // Target

		ListedHashTree testPlanTree = new ListedHashTree();
		testPlanTree.add(testPlan);
		testPlanTree.add(threadGroup1, testPlan);

		this.treeModel = new JMeterTreeModel(new Object());

		JMeterTreeNode root = (JMeterTreeNode) treeModel.getRoot();
		treeModel.addSubTree(testPlanTree, root);

		proxy = new ProxyControl();
		proxy.setNonGuiTreeModel(treeModel);
		proxy.setTarget(treeModel.getNodeOf(threadGroup1));
		proxy.setGroupingMode(2); // GROUPING_IN_SIMPLE_CONTROLLERS = 2
		proxy.setSamplerTypeName("2"); // SAMPLER_TYPE_HTTP_SAMPLER_HC4 = "2"
		proxy.setRegexMatch(Boolean.TRUE);
		proxy.setPort(Constants.PORT); // Global Settings -> Port
		proxy.setCaptureHttpHeaders(Boolean.TRUE);
		proxy.setSamplerFollowRedirects(Boolean.TRUE);

		treeModel.addComponent(proxy, (JMeterTreeNode) root.getChildAt(1));

	}

	public Boolean start() {
		try {
			this.proxy.startProxy();
		} catch (Exception e) {
			return Boolean.FALSE;
		}

		return Boolean.TRUE;
	}

	public File stop() {
		this.proxy.stopProxy();

		File jmxFile = null;

		if (this.treeModel == null) {
			return null;
		}

		try {
			String fileName = new SimpleDateFormat("yyyyMMddhhmmss'.jmx'").format(new Date());

			jmxFile = new File(fileName);

			HashTree tree = treeModel.getTestPlan();

			this.convertSubTree(tree);

			SaveService.saveTree(tree, new FileOutputStream(jmxFile));
		} catch (Exception e) {
			// TODO Auto-generated catch block
			// e.printStackTrace();
			return null;
		}

		return jmxFile;
	}

	private void convertSubTree(HashTree tree) {
		for (Object o : new LinkedList<>(tree.list())) {
			JMeterTreeNode item = (JMeterTreeNode) o;
			convertSubTree(tree.getTree(item));
			TestElement testElement = item.getTestElement(); // requires
																// JMeterTreeNode
			tree.replaceKey(item, testElement);
		}
	}

}
