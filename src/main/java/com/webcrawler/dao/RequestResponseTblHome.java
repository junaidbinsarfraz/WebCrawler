package com.webcrawler.dao;
// Generated May 11, 2017 2:36:17 PM by Hibernate Tools 5.1.0.Alpha1

import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.LockMode;
import org.hibernate.SessionFactory;
import org.hibernate.cache.impl.NoCachingRegionFactory;
import org.hibernate.cfg.Configuration;
import org.hibernate.cfg.Environment;
import org.hibernate.criterion.Example;

/**
 * Home object for domain model class RequestResponseTbl.
 * @see com.webcrawler.dao.RequestResponseTbl
 * @author Hibernate Tools
 */
public class RequestResponseTblHome {

	private static final Log log = LogFactory.getLog(RequestResponseTblHome.class);

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

	public void persist(RequestResponseTbl transientInstance) {
		log.debug("persisting RequestResponseTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().persist(transientInstance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("persist successful");
		} catch (RuntimeException re) {
			log.error("persist failed", re);
			sessionFactory.getCurrentSession().getTransaction().rollback();
			throw re;
		}
	}

	public void attachDirty(RequestResponseTbl instance) {
		log.debug("attaching dirty RequestResponseTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().saveOrUpdate(instance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("attach successful");
		} catch (RuntimeException re) {
			log.error("attach failed", re);
			sessionFactory.getCurrentSession().getTransaction().rollback();
			throw re;
		}
	}

	public void attachClean(RequestResponseTbl instance) {
		log.debug("attaching clean RequestResponseTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().lock(instance, LockMode.NONE);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("attach successful");
		} catch (RuntimeException re) {
			log.error("attach failed", re);
			sessionFactory.getCurrentSession().getTransaction().rollback();
			throw re;
		}
	}

	public void delete(RequestResponseTbl persistentInstance) {
		log.debug("deleting RequestResponseTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			sessionFactory.getCurrentSession().delete(persistentInstance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("delete successful");
		} catch (RuntimeException re) {
			log.error("delete failed", re);
			sessionFactory.getCurrentSession().getTransaction().rollback();
			throw re;
		}
	}

	public RequestResponseTbl merge(RequestResponseTbl detachedInstance) {
		log.debug("merging RequestResponseTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			RequestResponseTbl result = (RequestResponseTbl) sessionFactory.getCurrentSession().merge(detachedInstance);
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("merge successful");
			return result;
		} catch (RuntimeException re) {
			log.error("merge failed", re);
			sessionFactory.getCurrentSession().getTransaction().rollback();
			throw re;
		}
	}

	public RequestResponseTbl findById(java.lang.Integer id) {
		log.debug("getting RequestResponseTbl instance with id: " + id);
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			RequestResponseTbl instance = (RequestResponseTbl) sessionFactory.getCurrentSession().get("com.webcrawler.dao.RequestResponseTbl", id);
			if (instance == null) {
				log.debug("get successful, no instance found");
				sessionFactory.getCurrentSession().getTransaction().commit();
			} else {
				log.debug("get successful, instance found");
			}
			return instance;
		} catch (RuntimeException re) {
			log.error("get failed", re);
			sessionFactory.getCurrentSession().getTransaction().rollback();
			throw re;
		}
	}

	public List findByExample(RequestResponseTbl instance) {
		log.debug("finding RequestResponseTbl instance by example");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			List results = sessionFactory.getCurrentSession().createCriteria("com.webcrawler.dao.RequestResponseTbl").add(Example.create(instance))
					.list();
			log.debug("find by example successful, result size: " + results.size());
			sessionFactory.getCurrentSession().getTransaction().commit();
			return results;
		} catch (RuntimeException re) {
			log.error("find by example failed", re);
			sessionFactory.getCurrentSession().getTransaction().rollback();
			throw re;
		}
	}
	
	public List<RequestResponseTbl> findByRunId(Integer runId) {
		log.debug("finding RequestResponseTbl instance by example");
		try {
			sessionFactory.getCurrentSession().beginTransaction();

			List<RequestResponseTbl> results = sessionFactory.getCurrentSession()
					.createSQLQuery("select * from request_response_tbl r where r.RunId = " + runId).addEntity(RequestResponseTbl.class).list();

			log.debug("find by example successful, result size: " + results.size());
			sessionFactory.getCurrentSession().getTransaction().commit();
			return results;
		} catch (RuntimeException re) {
			log.error("find by example failed", re);
			sessionFactory.getCurrentSession().getTransaction().rollback();
			throw re;
		}
	}
}
