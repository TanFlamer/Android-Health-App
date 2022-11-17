package com.example.test.databaseFiles.dao;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import com.example.test.databaseFiles.entity.Sport;

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
    List<Sport> findSport(int sportID);

    @Query("SELECT * FROM Sport")
    List<Sport> getAllSport();
}
