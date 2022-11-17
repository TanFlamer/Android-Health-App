package com.example.test.databaseFiles.dao;

import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import com.example.test.databaseFiles.entity.SongPlaylist;

import java.util.List;

@Dao
public interface SongPlaylistDao {

    @Insert
    void insert(SongPlaylist songPlaylist);

    @Update
    void update(SongPlaylist songPlaylist);

    @Delete
    void delete(SongPlaylist songPlaylist);

    @Query("SELECT * FROM SongPlaylist WHERE playlistID=:playlistID AND songID=:songID")
    List<SongPlaylist> findSongPlaylist(int playlistID, int songID);

    @Query("SELECT * FROM SongPlaylist")
    List<SongPlaylist> getAllSongPlaylist();
}
