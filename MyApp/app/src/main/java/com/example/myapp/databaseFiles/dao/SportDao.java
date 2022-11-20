package com.example.myapp.databaseFiles.dao;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.TypeConverters;
import androidx.room.Update;

import com.example.myapp.databaseFiles.converter.DateConverter;
import com.example.myapp.databaseFiles.entity.Sport;

import java.time.LocalDate;
import java.util.List;

@Dao
public interface SportDao {

    @Insert
    void insert(Sport sport);

    @Update
    void update(Sport sport);

    @Delete
    void delete(Sport sport);

    @Query("SELECT * FROM Sport WHERE sportID=:sportID")
    List<Sport> getSport(int sportID);

    @Query("SELECT * FROM Sport WHERE userID=:userID AND date=:date")
    @TypeConverters(DateConverter.class)
    List<Sport> findSport(int userID, LocalDate date);

    @Query("SELECT * FROM Sport WHERE userID=:userID")
    LiveData<List<Sport>> getAllSport(int userID);
}
