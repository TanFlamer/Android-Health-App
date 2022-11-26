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
    void insert(Sleep sleep);

    @Update
    void update(Sleep sleep);

    @Delete
    void delete(Sleep sleep);

    @Query("SELECT * FROM Sleep WHERE userID=:userID AND date=:date")
    List<Sleep> findSleep(int userID, long date);

    @Query("SELECT * FROM Sleep WHERE userID=:userID")
    LiveData<List<Sleep>> getAllSleep(int userID);
}
