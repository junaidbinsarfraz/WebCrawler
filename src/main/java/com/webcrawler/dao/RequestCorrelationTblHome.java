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
 * Home object for domain model class RequestCorrelationTbl.
 * @see com.webcrawler.dao.RequestCorrelationTbl
 * @author Hibernate Tools
 */
public class RequestCorrelationTblHome {

	private static final Log log = LogFactory.getLog(RequestCorrelationTblHome.class);

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

	public void persist(RequestCorrelationTbl transientInstance) {
		log.debug("persisting RequestCorrelationTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().persist(transientInstance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("persist successful");
		} catch (RuntimeException re) {
			log.error("persist failed", re);
			throw re;
		}
	}

	public void attachDirty(RequestCorrelationTbl instance) {
		log.debug("attaching dirty RequestCorrelationTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().saveOrUpdate(instance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("attach successful");
		} catch (RuntimeException re) {
			log.error("attach failed", re);
			throw re;
		}
	}

	public void attachClean(RequestCorrelationTbl instance) {
		log.debug("attaching clean RequestCorrelationTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().lock(instance, LockMode.NONE);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("attach successful");
		} catch (RuntimeException re) {
			log.error("attach failed", re);
			throw re;
		}
	}

	public void delete(RequestCorrelationTbl persistentInstance) {
		log.debug("deleting RequestCorrelationTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().delete(persistentInstance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("delete successful");
		} catch (RuntimeException re) {
			log.error("delete failed", re);
			throw re;
		}
	}

	public RequestCorrelationTbl merge(RequestCorrelationTbl detachedInstance) {
		log.debug("merging RequestCorrelationTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			RequestCorrelationTbl result = (RequestCorrelationTbl) sessionFactory.getCurrentSession().merge(detachedInstance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("merge successful");
			return result;
		} catch (RuntimeException re) {
			log.error("merge failed", re);
			throw re;
		}
	}

	public RequestCorrelationTbl findById(java.lang.Integer id) {
		log.debug("getting RequestCorrelationTbl instance with id: " + id);
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			RequestCorrelationTbl instance = (RequestCorrelationTbl) sessionFactory.getCurrentSession()
					.get("com.webcrawler.dao.RequestCorrelationTbl", id);
			if (instance == null) {
				log.debug("get successful, no instance found");
				sessionFactory.getCurrentSession().getTransaction().commit();
			} else {
				log.debug("get successful, instance found");
			}
			return instance;
		} catch (RuntimeException re) {
			log.error("get failed", re);
			throw re;
		}
	}

	public List findByExample(RequestCorrelationTbl instance) {
		log.debug("finding RequestCorrelationTbl instance by example");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			List results = sessionFactory.getCurrentSession().createCriteria("com.webcrawler.dao.RequestCorrelationTbl").add(Example.create(instance))
					.list();
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("find by example successful, result size: " + results.size());
			return results;
		} catch (RuntimeException re) {
			log.error("find by example failed", re);
			throw re;
		}
	}
	
	public List findByRunId(Integer runId) {
		log.debug("finding findByRunId instance by example");
		
		if(Util.isNull(runId)) {
			return new ArrayList<>();
		}
		
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			
			String query = "select * from request_correlation_tbl where RunId = '" + runId + "'";
			
			List results = sessionFactory.getCurrentSession().createSQLQuery(query).addEntity(RequestCorrelationTbl.class).list();
			log.debug("find by run id successful, result size: " + results.size());
			sessionFactory.getCurrentSession().getTransaction().commit();
			return results;
		} catch (RuntimeException re) {
			log.error("find by run id failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}
}
