package com.example.myapp.databaseFiles.sportschedule;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import java.util.List;

@Dao
public interface SportScheduleDao {

    @Insert
    void insert(SportSchedule sportSchedule); //insert operation for new sport schedule

    @Update
    void update(SportSchedule sportSchedule); //update operation for new sport schedule

    @Delete
    void delete(SportSchedule sportSchedule); //delete operation for new sport schedule

    @Query("SELECT * FROM SportSchedule WHERE userID=:userID")
    LiveData<List<SportSchedule>> getAllSportSchedule(int userID); //returns live data of all sport schedules belonging to a user
}
