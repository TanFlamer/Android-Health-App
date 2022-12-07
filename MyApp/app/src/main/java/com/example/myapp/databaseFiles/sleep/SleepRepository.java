package com.example.myapp.databasefiles.sleep;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;

import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SleepRepository {

    //sleep data data access object
    private final SleepDao sleepDao;

    //constructor for sleep data repository
    public SleepRepository(Application application) {
        Database database = Database.getInstance(application);
        sleepDao = database.getSleepDao();
    }

    //insert operation for sleep data repository
    public void insert(Sleep sleep) {
        new InsertSleepExecutorTask(sleepDao).execute(sleep);
    }

    //update operation for sleep data repository
    public void update(Sleep sleep) {
        new UpdateSleepExecutorTask(sleepDao).execute(sleep);
    }

    //delete operation for sleep data repository
    public void delete(Sleep sleep) {
        new DeleteSleepExecutorTask(sleepDao).execute(sleep);
    }

    //check if sleep data with specific date exists for a user
    public Sleep findSleep(int userID, long date) {
        return new FindSleepExecutorTask(sleepDao).find(userID, date);
    }

    //returns live data of all sleep data belonging to a user
    public LiveData<List<Sleep>> getAllSleep(int userID) {
        return sleepDao.getAllSleep(userID);
    }

    //insert sleep data executor task
    private static class InsertSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SleepDao sleepDao;
        private InsertSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.insert(sleep));
        }
    }

    //update sleep data executor task
    private static class UpdateSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SleepDao sleepDao;
        private UpdateSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.update(sleep));
        }
    }

    //delete sleep data executor task
    private static class DeleteSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SleepDao sleepDao;
        private DeleteSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected void execute(Sleep sleep){
            service.execute(() -> sleepDao.delete(sleep));
        }
    }

    //find sleep data executor task
    private static class FindSleepExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SleepDao sleepDao;
        private FindSleepExecutorTask(SleepDao sleepDao) {
            this.sleepDao = sleepDao;
        }
        protected Sleep find(int userID, long date) {
            try {
                return service.submit(() -> sleepDao.findSleep(userID, date)).get();
            } catch (ExecutionException | InterruptedException e) {
                e.printStackTrace();
            }
            return null;
        }
    }
}
