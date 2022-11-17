package com.example.myapp.databaseFiles.dao;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.TypeConverters;
import androidx.room.Update;

import com.example.myapp.databaseFiles.converter.DateConverter;
import com.example.myapp.databaseFiles.entity.Sleep;

import java.time.LocalDate;
import java.util.List;

@Dao
public interface SleepDao {

    @Insert
    void insert(Sleep sleep);

    @Update
    void update(Sleep sleep);

    @Delete
    void delete(Sleep sleep);

    @Query("SELECT * FROM Sleep WHERE userID=:userID AND date=:date")
    @TypeConverters(DateConverter.class)
    List<Sleep> findSleep(int userID, LocalDate date);

    @Query("SELECT * FROM Sleep WHERE userID=:userID")
    LiveData<List<Sleep>> getAllSleep(int userID);
}
