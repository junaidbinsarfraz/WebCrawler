package com.webcrawler.dao;
// Generated Sep 12, 2017 10:44:30 AM by Hibernate Tools 5.1.0.Alpha1

import java.util.List;
import javax.naming.InitialContext;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.LockMode;
import org.hibernate.SessionFactory;
import org.hibernate.cache.impl.NoCachingRegionFactory;
import org.hibernate.cfg.Configuration;
import org.hibernate.cfg.Environment;
import org.hibernate.criterion.Example;

/**
 * Home object for domain model class KnownExtractorsTbl.
 * @see com.webcrawler.dao.KnownExtractorsTbl
 * @author Hibernate Tools
 */
public class KnownExtractorsTblHome {

	private static final Log log = LogFactory.getLog(KnownExtractorsTblHome.class);

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

	public void persist(KnownExtractorsTbl transientInstance) {
		log.debug("persisting KnownExtractorsTbl instance");
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

	public void attachDirty(KnownExtractorsTbl instance) {
		log.debug("attaching dirty KnownExtractorsTbl instance");
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

	public void attachClean(KnownExtractorsTbl instance) {
		log.debug("attaching clean KnownExtractorsTbl instance");
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

	public void delete(KnownExtractorsTbl persistentInstance) {
		log.debug("deleting KnownExtractorsTbl instance");
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

	public KnownExtractorsTbl merge(KnownExtractorsTbl detachedInstance) {
		log.debug("merging KnownExtractorsTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			KnownExtractorsTbl result = (KnownExtractorsTbl) sessionFactory.getCurrentSession().merge(detachedInstance);
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

	public KnownExtractorsTbl findById(java.lang.Integer id) {
		log.debug("getting KnownExtractorsTbl instance with id: " + id);
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			KnownExtractorsTbl instance = (KnownExtractorsTbl) sessionFactory.getCurrentSession().get("com.webcrawler.dao.KnownExtractorsTbl", id);
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

	public List findByExample(KnownExtractorsTbl instance) {
		log.debug("finding KnownExtractorsTbl instance by example");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			List results = sessionFactory.getCurrentSession().createCriteria("com.webcrawler.dao.KnownExtractorsTbl").add(Example.create(instance))
					.list();
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
	
	public List getAll() {
		log.debug("getAll KnownExtractorsTbl instances");
		try {
			sessionFactory.getCurrentSession().beginTransaction();

			String query = "select * from known_extractors_tbl";
			
			List results = sessionFactory.getCurrentSession().createSQLQuery(query).addEntity(KnownExtractorsTbl.class).list();
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("getAll KnownExtractorsTbl instances successful, result size: " + results.size());
			return results;
		} catch (RuntimeException re) {
			log.error("getAll KnownExtractorsTbl instances failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}
}
