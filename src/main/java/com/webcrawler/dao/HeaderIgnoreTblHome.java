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
 * Home object for domain model class HeaderIgnoreTbl.
 * @see com.webcrawler.dao.HeaderIgnoreTbl
 * @author Hibernate Tools
 */
public class HeaderIgnoreTblHome {

	private static final Log log = LogFactory.getLog(HeaderIgnoreTblHome.class);

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

	public void persist(HeaderIgnoreTbl transientInstance) {
		log.debug("persisting HeaderIgnoreTbl instance");
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

	public void attachDirty(HeaderIgnoreTbl instance) {
		log.debug("attaching dirty HeaderIgnoreTbl instance");
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

	public void attachClean(HeaderIgnoreTbl instance) {
		log.debug("attaching clean HeaderIgnoreTbl instance");
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

	public void delete(HeaderIgnoreTbl persistentInstance) {
		log.debug("deleting HeaderIgnoreTbl instance");
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

	public HeaderIgnoreTbl merge(HeaderIgnoreTbl detachedInstance) {
		log.debug("merging HeaderIgnoreTbl instance");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			HeaderIgnoreTbl result = (HeaderIgnoreTbl) sessionFactory.getCurrentSession().merge(detachedInstance);
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

	public HeaderIgnoreTbl findById(java.lang.Integer id) {
		log.debug("getting HeaderIgnoreTbl instance with id: " + id);
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			HeaderIgnoreTbl instance = (HeaderIgnoreTbl) sessionFactory.getCurrentSession().get("com.webcrawler.dao.HeaderIgnoreTbl", id);
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

	public List findByExample(HeaderIgnoreTbl instance) {
		log.debug("finding HeaderIgnoreTbl instance by example");
		try {
			sessionFactory.getCurrentSession().beginTransaction();
			List results = sessionFactory.getCurrentSession().createCriteria("com.webcrawler.dao.HeaderIgnoreTbl").add(Example.create(instance))
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
		log.debug("getAll HeaderIgnoreTbl instances");
		try {
			sessionFactory.getCurrentSession().beginTransaction();

			String query = "select * from header_ignore_tbl";
			
			List results = sessionFactory.getCurrentSession().createSQLQuery(query).addEntity(HeaderIgnoreTbl.class).list();
			sessionFactory.getCurrentSession().getTransaction().commit();
			log.debug("getAll HeaderIgnoreTbl instances successful, result size: " + results.size());
			return results;
		} catch (RuntimeException re) {
			log.error("getAll HeaderIgnoreTbl instances failed", re);
			sessionFactory.getCurrentSession().clear();
			sessionFactory.getCurrentSession().flush();
			throw re;
		}
	}
}
