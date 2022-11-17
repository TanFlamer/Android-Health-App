package com.example.test.databaseFiles.repository;

import android.app.Application;

import com.example.test.databaseFiles.Database;
import com.example.test.databaseFiles.dao.SleepDao;
import com.example.test.databaseFiles.entity.Sleep;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SleepRepository {

    private SleepDao sleepDao;
    private List<Sleep> allSleep;

    public SleepRepository(Application application) {
        Database database = Database.getInstance(application);
        sleepDao = database.getSleepDao();
        allSleep = sleepDao.getAllSleep();
    }

    public void insert(Sleep sleep) {
        new InsertSleepExecutorTask(sleepDao).execute(sleep);
    }

    public void update(Sleep sleep) {
        new UpdateSleepExecutorTask(sleepDao).execute(sleep);
    }

    public void delete(Sleep sleep) {
        new DeleteSleepExecutorTask(sleepDao).execute(sleep);
    }

    public List<Sleep> findSleep(int sleepID) {
        return new FindSleepExecutorTask(sleepDao).get(sleepID);
    }

    public List<Sleep> getAllSleep() {
        return allSleep;
    }

    private static class InsertSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private InsertSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.insert(sleep));
        }
    }

    private static class UpdateSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private UpdateSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.update(sleep));
        }
    }

    private static class DeleteSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private DeleteSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.delete(sleep));
        }
    }

    private static class FindSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private SleepDao sleepDao;
        private FindSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected List<Sleep> get(int sleepID) {
            try {
                return service.submit(() -> sleepDao.findSleep(sleepID)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
