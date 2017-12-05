package com.webcrawler.dao;
// Generated May 11, 2017 2:36:17 PM by Hibernate Tools 5.1.0.Alpha1

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.LockMode;
import org.hibernate.SessionFactory;
import org.hibernate.cache.impl.NoCachingRegionFactory;
import org.hibernate.cfg.Configuration;
import org.hibernate.cfg.Environment;
import org.hibernate.criterion.Example;

import com.webcrawler.common.util.Util;

/**
 * Home object for domain model class RunIdentTbl.
 * @see com.webcrawler.dao.RunIdentTbl
 * @author Hibernate Tools
 */
public class RunIdentTblHome {

	private static final Log log = LogFactory.getLog(RunIdentTblHome.class);

	private final SessionFactory sessionFactory = getSessionFactory();

	protected SessionFactory getSessionFactory() {
		try {
			Configuration configuration = new Configuration().configure(
                    "hibernate.cfg.xml");
			configuration.setProperty( Environment.USE_QUERY_CACHE, Boolean.FALSE.toString() );
			configuration.setProperty( Environment.USE_SECOND_LEVEL_CACHE, Boolean.FALSE.toString() );
			configuration.setProperty(Environment.CACHE_REGION_FACTORY, NoCachingRegionFactory.class.getName());
//			configuration.setProperty(Environment.CACHE_PROVIDER_CONFIG,NoCachingRegionFactory.class.getName());	
            SessionFactory sessionFactory = configuration.buildSessionFactory();

            return sessionFactory;

        } catch (Exception e) {

            log.error("Initial SessionFactory creation failed." + e);
            throw new IllegalStateException("Initial Session Factory creation failed.");
        }
	}

	public void persist(RunIdentTbl transientInstance) {
		log.debug("persisting RunIdentTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().persist(transientInstance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("persist successful");
		} catch (RuntimeException re) {
			log.error("persist failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}

	public void attachDirty(RunIdentTbl instance) {
		log.debug("attaching dirty RunIdentTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().saveOrUpdate(instance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("attach successful");
		} catch (RuntimeException re) {
			log.error("attach failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}

	public void attachClean(RunIdentTbl instance) {
		log.debug("attaching clean RunIdentTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().lock(instance, LockMode.NONE);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("attach successful");
		} catch (RuntimeException re) {
			log.error("attach failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}

	public void delete(RunIdentTbl persistentInstance) {
		log.debug("deleting RunIdentTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().delete(persistentInstance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("delete successful");
		} catch (RuntimeException re) {
			log.error("delete failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}

	public RunIdentTbl merge(RunIdentTbl detachedInstance) {
		log.debug("merging RunIdentTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			RunIdentTbl result = (RunIdentTbl) sessionFactory.getCurrentSession().merge(detachedInstance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("merge successful");
			return result;
		} catch (RuntimeException re) {
			log.error("merge failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}

	public RunIdentTbl findById(java.lang.Integer id) {
		log.debug("getting RunIdentTbl instance with id: " + id);
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			RunIdentTbl instance = (RunIdentTbl) sessionFactory.getCurrentSession().get("com.webcrawler.dao.RunIdentTbl", id);
			if (instance == null) {
				log.debug("get successful, no instance found");
				sessionFactory.getCurrentSession().getTransaction().commit();
			} else {
				log.debug("get successful, instance found");
			}
			return instance;
		} catch (RuntimeException re) {
			log.error("get failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}

	public List findByExample(RunIdentTbl instance) {
		log.debug("finding RunIdentTbl instance by example");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			List results = sessionFactory.getCurrentSession().createCriteria("com.webcrawler.dao.RunIdentTbl").add(Example.create(instance)).list();
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("find by example successful, result size: " + results.size());
			return results;
		} catch (RuntimeException re) {
			log.error("find by example failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}
	
	public List findByRunName(String runName) {
		log.debug("finding findByRunName instance by example");
		
		if(Util.isNullOrEmpty(runName)) {
			return new ArrayList<>();
		}
		
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			
			String query = "select * from run_ident_tbl where RunIdentifier = '" + runName + "'";
			
			List results = sessionFactory.getCurrentSession().createSQLQuery(query).addEntity(RunIdentTbl.class).list();
			log.debug("find by example successful, result size: " + results.size());
			sessionFactory.getCurrentSession().getTransaction().commit();
			return results;
		} catch (RuntimeException re) {
			log.error("find by run name failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}
}
