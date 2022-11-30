package com.example.myapp.databasefiles.sport;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import java.util.List;

@Dao
public interface SportDao {

    @Insert
    long insert(Sport sport);

    @Update
    void update(Sport sport);

    @Delete
    void delete(Sport sport);

    @Query("SELECT * FROM Sport WHERE sportID=:sportID")
    Sport getSport(int sportID);

    @Query("SELECT * FROM Sport WHERE userID=:userID AND date=:date")
    Sport findSport(int userID, long date);

    @Query("SELECT * FROM Sport WHERE userID=:userID")
    LiveData<List<Sport>> getAllSport(int userID);
}
