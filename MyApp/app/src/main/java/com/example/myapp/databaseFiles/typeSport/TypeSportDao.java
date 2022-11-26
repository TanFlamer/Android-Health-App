package com.example.myapp.databaseFiles.typeSport;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

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
    List<TypeSport> getTypeSport(int sportID);

    @Query("SELECT * FROM TypeSport WHERE userID=:userID")
    LiveData<List<TypeSport>> getAllTypeSport(int userID);
}
