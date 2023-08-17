package com.example.myapp.databaseFiles.sleep;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import java.util.List;

@Dao
public interface SleepDao {

    @Insert
    void insert(Sleep sleep); //insert operation for new sleep data

    @Update
    void update(Sleep sleep); //update operation for existing sleep data

    @Delete
    void delete(Sleep sleep); //delete operation for existing sleep data

    @Query("SELECT * FROM Sleep WHERE userID=:userID AND date=:date")
    Sleep findSleep(int userID, long date); //check if sleep data with specific date exists for a user

    @Query("SELECT * FROM Sleep WHERE userID=:userID")
    LiveData<List<Sleep>> getAllSleep(int userID); //returns live data of all sleep data belonging to a user
}
