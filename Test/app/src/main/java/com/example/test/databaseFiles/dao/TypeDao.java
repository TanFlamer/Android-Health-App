package com.example.test.databaseFiles.dao;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import com.example.test.databaseFiles.entity.Type;

import java.util.List;

@Dao
public interface TypeDao {

    @Insert
    void insert(Type type);

    @Update
    void update(Type type);

    @Delete
    void delete(Type type);

    @Query("SELECT * FROM Types WHERE typeID=:typeID")
    List<Type> findType(int typeID);

    @Query("SELECT * FROM Types WHERE userID=:userID")
    LiveData<List<Type>> getAllTypes(int userID);
}
