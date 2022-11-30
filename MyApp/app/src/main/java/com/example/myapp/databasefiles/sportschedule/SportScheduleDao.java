package com.example.myapp.databasefiles.sportschedule;

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
    void insert(SportSchedule sportSchedule);

    @Update
    void update(SportSchedule sportSchedule);

    @Delete
    void delete(SportSchedule sportSchedule);

    @Query("SELECT * FROM SportSchedule WHERE userID=:userID")
    LiveData<List<SportSchedule>> getAllSportSchedule(int userID);
}
