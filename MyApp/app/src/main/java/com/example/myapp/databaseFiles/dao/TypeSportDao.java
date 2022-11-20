package com.example.myapp.databaseFiles.dao;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import com.example.myapp.databaseFiles.entity.TypeSport;

import java.util.List;

@Dao
public interface TypeSportDao {

    @Insert
    void insert(TypeSport typeSport);

    @Update
    void update(TypeSport typeSport);

    @Delete
    void delete(TypeSport typeSport);

    @Query("SELECT * FROM TypeSport WHERE sportID=:sportID AND typeID=:typeID")
    List<TypeSport> findTypeSport(int sportID, int typeID);

    @Query("SELECT * FROM TypeSport WHERE sportID=:sportID")
    LiveData<List<TypeSport>> getTypeSport(int sportID);

    @Query("SELECT * FROM TypeSport WHERE userID=:userID")
    LiveData<List<TypeSport>> getAllTypeSport(int userID);
}
