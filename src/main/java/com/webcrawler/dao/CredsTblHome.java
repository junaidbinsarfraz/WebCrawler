package com.webcrawler.dao;
// Generated Sep 12, 2017 10:44:30 AM by Hibernate Tools 5.1.0.Alpha1

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
 * Home object for domain model class CredsTbl.
 * @see com.webcrawler.dao.CredsTbl
 * @author Hibernate Tools
 */
public class CredsTblHome {

	private static final Log log = LogFactory.getLog(CredsTblHome.class);

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

	public void persist(CredsTbl transientInstance) {
		log.debug("persisting CredsTbl instance");
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

	public void attachDirty(CredsTbl instance) {
		log.debug("attaching dirty CredsTbl instance");
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

	public void attachClean(CredsTbl instance) {
		log.debug("attaching clean CredsTbl instance");
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

	public void delete(CredsTbl persistentInstance) {
		log.debug("deleting CredsTbl instance");
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

	public CredsTbl merge(CredsTbl detachedInstance) {
		log.debug("merging CredsTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			CredsTbl result = (CredsTbl) sessionFactory.getCurrentSession().merge(detachedInstance);
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

	public CredsTbl findById(java.lang.Integer id) {
		log.debug("getting CredsTbl instance with id: " + id);
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			CredsTbl instance = (CredsTbl) sessionFactory.getCurrentSession().get("com.webcrawler.dao.CredsTbl", id);
			if (instance == null) {
				sessionFactory.getCurrentSession().getTransaction().commit();
				log.debug("get successful, no instance found");
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

	public List findByExample(CredsTbl instance) {
		log.debug("finding CredsTbl instance by example");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			List results = sessionFactory.getCurrentSession().createCriteria("com.webcrawler.dao.CredsTbl").add(Example.create(instance)).list();
			log.debug("find by example successful, result size: " + results.size());
			sessionFactory.getCurrentSession().getTransaction().commit();
			return results;
		} catch (RuntimeException re) {
			log.error("find by example failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}
	
	public List<CredsTbl> findByRunId(Integer runId) {
		log.debug("finding CredsTbl instance by example");
		
		if(Util.isNull(runId)) {
			return new ArrayList<CredsTbl>();
		}
		
		try {
			sessionFactory.getCurrentSession().beginTransaction();

			List<CredsTbl> results = sessionFactory.getCurrentSession()
					.createSQLQuery("select * from creds_tbl c where c.RunId = " + runId).addEntity(CredsTbl.class).list();

			log.debug("find by example successful, result size: " + results.size());
			sessionFactory.getCurrentSession().getTransaction().commit();
			return results;
		} catch (RuntimeException re) {
			log.error("find by example failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}
}
