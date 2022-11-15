package com.example.myapp.databaseFiles.dao;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import com.example.myapp.databaseFiles.entity.Sleep;

import java.util.List;

@Dao
public interface SleepDao {

    @Insert
    void insert(Sleep sleep);

    @Update
    void update(Sleep sleep);

    @Delete
    void delete(Sleep sleep);

    @Query("SELECT * FROM Sleep WHERE sleepID=:sleepID")
    Sleep findSleep(int sleepID);

    @Query("SELECT * FROM Sleep")
    List<Sleep> getAllSleep();
}
