package com.example.myapp.databaseFiles.dao;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import com.example.myapp.databaseFiles.entity.Type;

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
    Type findType(int typeID);

    @Query("SELECT * FROM Types")
    List<Type> getAllTypes();
}
