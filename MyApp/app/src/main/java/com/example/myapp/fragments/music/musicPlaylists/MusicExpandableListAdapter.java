package com.example.myapp.fragments.music.musicPlaylists;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.playlist.Playlist;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.subActivities.music.DataMusic;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class MusicExpandableListAdapter extends BaseExpandableListAdapter {

    private Context context;
    private List<Playlist> playlistList;
    private HashMap<Playlist, List<Song>> songPlaylists;

    public MusicExpandableListAdapter(Context context, HashMap<Playlist, List<Song>> songPlaylists){
        this.context = context;
        this.playlistList = new ArrayList<>(songPlaylists.keySet());
        this.songPlaylists = songPlaylists;
    }

    @Override
    public int getGroupCount() {
        return playlistList.size();
    }

    @Override
    public int getChildrenCount(int i) {
        return Objects.requireNonNull(songPlaylists.get(playlistList.get(i))).size();
    }

    @Override
    public Object getGroup(int i) {
        return songPlaylists.get(playlistList.get(i));
    }

    @Override
    public Object getChild(int i, int i1) {
        return Objects.requireNonNull(songPlaylists.get(playlistList.get(i))).get(i1);
    }

    @Override
    public long getGroupId(int i) {
        return i;
    }

    @Override
    public long getChildId(int i, int i1) {
        return i1;
    }

    @Override
    public boolean hasStableIds() {
        return true;
    }

    @SuppressLint("InflateParams")
    @Override
    public View getGroupView(int i, boolean b, View view, ViewGroup viewGroup) {
        String playlistName = playlistList.get(i).getPlaylistName();

        if(view == null)
            view = LayoutInflater.from(context).inflate(R.layout.music_expandable_list_item, null);

        TextView nameView = view.findViewById(R.id.musicPlaylistName);
        ImageView clickEdit = view.findViewById(R.id.clickEdit);
        ImageView clickDelete = view.findViewById(R.id.clickDelete);
        nameView.setText(playlistName);
        clickEdit.setOnClickListener(v -> {
            Intent intent = new Intent(context, DataMusic.class);
            intent.putExtra("playlistName", playlistName);
            context.startActivity(intent);
        });

        return view;
    }

    @SuppressLint({"InflateParams", "SetTextI18n"})
    @Override
    public View getChildView(int i, int i1, boolean b, View view, ViewGroup viewGroup) {
        Song song = Objects.requireNonNull(songPlaylists.get(playlistList.get(i))).get(i1);

        if(view == null)
            view = LayoutInflater.from(context).inflate(R.layout.music_expandable_list_item_data, null);

        TextView nameView = view.findViewById(R.id.musicSongName);
        TextView lengthView = view.findViewById(R.id.musicSongLength);

        nameView.setText(song.getSongName());
        lengthView.setText(song.getSongDuration().toString());

        return view;
    }

    @Override
    public boolean isChildSelectable(int i, int i1) {
        return true;
    }

    public void updateMusicPlaylists(HashMap<Playlist, List<Song>> newSongPlaylists, String data, String order){
        playlistList.clear();
        playlistList.addAll(newSongPlaylists.keySet());
        songPlaylists.clear();
        songPlaylists.putAll(newSongPlaylists);
        sortMusicPlaylists(data, order);
    }

    public void sortMusicPlaylists(String data, String order){
        playlistList.sort(getPlaylistComparator(data, order));
        for(List<Song> songList : songPlaylists.values()) songList.sort(getSongComparator(data, order));
        notifyDataSetChanged();
    }

    public Comparator<Playlist> getPlaylistComparator(String data, String order){
        Comparator<Playlist> playlistComparator = Comparator.comparingInt(Playlist::getPlaylistID);
        switch (data) {
            case "Date Added":
                playlistComparator = Comparator.comparingInt(Playlist::getPlaylistID);
                break;
            case "Name":
                playlistComparator = Comparator.comparing(Playlist::getPlaylistName);
                break;
            case "Length":
                playlistComparator = Comparator.comparing(this::getPlaylistLength);
                break;
        }
        return order.equals("Ascending") ? playlistComparator : playlistComparator.reversed();
    }

    public Comparator<Song> getSongComparator(String data, String order){
        Comparator<Song> songComparator = Comparator.comparingInt(Song::getSongID);
        switch (data) {
            case "Date Added":
                songComparator = Comparator.comparingInt(Song::getSongID);
                break;
            case "Name":
                songComparator = Comparator.comparing(Song::getSongName);
                break;
            case "Length":
                songComparator = Comparator.comparing(Song::getSongDuration);
                break;
        }
        return order.equals("Ascending") ? songComparator : songComparator.reversed();
    }

    public int getPlaylistLength(Playlist playlist){
        int duration = 0;
        for(Song song : Objects.requireNonNull(songPlaylists.get(playlist)))
            duration += song.getSongDuration();
        return duration;
    }
}
