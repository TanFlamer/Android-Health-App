package com.example.myapp.databaseFiles.sportschedule;

import android.app.Application;

import androidx.lifecycle.LiveData;

import com.example.myapp.Database;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SportScheduleRepository {

    //sport schedule data access object
    private final SportScheduleDao sportScheduleDao;

    //constructor for sport schedule repository
    public SportScheduleRepository(Application application) {
        Database database = Database.getInstance(application);
        sportScheduleDao = database.getTypeSportDao();
    }

    //insert operation for sport schedule repository
    public void insert(SportSchedule sportSchedule) {
        new InsertTypeScheduleExecutorTask(sportScheduleDao).execute(sportSchedule);
    }

    //update operation for sport schedule repository
    public void update(SportSchedule sportSchedule) {
        new UpdateTypeScheduleExecutorTask(sportScheduleDao).execute(sportSchedule);
    }

    //delete operation for sport schedule repository
    public void delete(SportSchedule sportSchedule) {
        new DeleteTypeScheduleExecutorTask(sportScheduleDao).execute(sportSchedule);
    }

    //returns live data of all sport schedules belonging to a user
    public LiveData<List<SportSchedule>> getAllSportSchedule(int userID) {
        return sportScheduleDao.getAllSportSchedule(userID);
    }

    //insert sport schedule executor task
    private static class InsertTypeScheduleExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SportScheduleDao sportScheduleDao;
        private InsertTypeScheduleExecutorTask(SportScheduleDao sportScheduleDao) {
            this.sportScheduleDao = sportScheduleDao;
        }
        protected void execute(SportSchedule sportSchedule){
            service.execute(() -> sportScheduleDao.insert(sportSchedule));
        }
    }

    //update sport schedule executor task
    private static class UpdateTypeScheduleExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SportScheduleDao sportScheduleDao;
        private UpdateTypeScheduleExecutorTask(SportScheduleDao sportScheduleDao) {
            this.sportScheduleDao = sportScheduleDao;
        }
        protected void execute(SportSchedule sportSchedule){
            service.execute(() -> sportScheduleDao.update(sportSchedule));
        }
    }

    //delete sport schedule executor task
    private static class DeleteTypeScheduleExecutorTask {
        private final ExecutorService service = Executors.newSingleThreadExecutor();
        private final SportScheduleDao sportScheduleDao;
        private DeleteTypeScheduleExecutorTask(SportScheduleDao sportScheduleDao) {
            this.sportScheduleDao = sportScheduleDao;
        }
        protected void execute(SportSchedule sportSchedule){
            service.execute(() -> sportScheduleDao.delete(sportSchedule));
        }
    }
}
