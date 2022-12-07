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
    long insert(Sport sport); //insert operation for new sport data

    @Update
    void update(Sport sport); //update operation for existing sport data

    @Delete
    void delete(Sport sport); //delete operation for existing sport data

    @Query("SELECT * FROM Sport WHERE userID=:userID AND date=:date")
    Sport findSport(int userID, long date); //check if sport data with specific date exists for a user

    @Query("SELECT * FROM Sport WHERE userID=:userID")
    LiveData<List<Sport>> getAllSport(int userID); //returns live data of all sport data belonging to a user
}
