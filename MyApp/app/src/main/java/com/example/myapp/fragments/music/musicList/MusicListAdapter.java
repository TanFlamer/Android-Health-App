package com.example.myapp.fragments.music.musicList;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.example.myapp.MusicPlayer;
import com.example.myapp.R;
import com.example.myapp.databasefiles.song.Song;
import com.example.myapp.ViewHolder;

import java.util.HashMap;
import java.util.List;

public class MusicListAdapter extends ArrayAdapter<Song> {

    private final Context context;
    private final List<Song> songList;
    private final HashMap<Song, Boolean> buttonMap;
    private final MusicListViewModel musicListViewModel;

    //constructor for song list adapter
    public MusicListAdapter(@NonNull Context context, int resource, List<Song> songList, MusicListViewModel musicListViewModel) {
        super(context, resource, songList);
        this.context = context;
        this.songList = songList;
        this.musicListViewModel = musicListViewModel;
        buttonMap = new HashMap<>();
        for(Song song : songList) buttonMap.put(song, false);
    }

    @NonNull
    @Override //get view for each song
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View currentItemView = convertView;

        //inflate new view for song if null
        if(currentItemView == null) {
            currentItemView = LayoutInflater.from(getContext()).inflate(R.layout.music_list_item, parent, false);
            //create new view holder
            ViewHolder viewHolder = new ViewHolder();
            //add layout to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.layoutHidden));
            //add text view to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.musicName));
            //add text view to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.musicLength));
            //add button to view holder
            viewHolder.addView(currentItemView.findViewById(R.id.clickDelete));
            //set tag to view
            currentItemView.setTag(viewHolder);
        }

        //get view holder
        ViewHolder viewHolder = (ViewHolder) currentItemView.getTag();
        //update song view data
        updateAllViews(viewHolder, position);
        //return song view
        return currentItemView;
    }

    //initialise song view data
    public void updateAllViews(ViewHolder viewHolder, int position){
        Song song = songList.get(position);
        //update song hidden layout
        updateLayouts(viewHolder, song);
        //update song name
        updateNameView(viewHolder, song);
        //update song duration
        updateLengthView(viewHolder, song);
        //update song delete button
        updateDeleteButton(viewHolder, song);
    }

    //update song hidden layout
    public void updateLayouts(ViewHolder viewHolder, Song song){
        //get hidden layout by ID
        LinearLayout layoutHidden = (LinearLayout) viewHolder.getView(R.id.layoutHidden);
        //change visibility of hidden layout on long click
        layoutHidden.setVisibility(Boolean.TRUE.equals(buttonMap.get(song)) ? View.VISIBLE : View.GONE);
    }

    //update song name
    public void updateNameView(ViewHolder viewHolder, Song song){
        //get text view ID for song name
        TextView nameView = (TextView) viewHolder.getView(R.id.musicName);
        //set song name
        nameView.setText(song.getSongName());
    }

    //update song duration
    @SuppressLint("DefaultLocale")
    public void updateLengthView(ViewHolder viewHolder, Song song){
        //get text view ID for song duration
        TextView lengthView = (TextView) viewHolder.getView(R.id.musicLength);
        int length = song.getSongDuration();
        //set song duration
        lengthView.setText(String.format("%d:%02d", length/60, length%60));
    }

    //update song delete button
    public void updateDeleteButton(ViewHolder viewHolder, Song song){
        //get delete button by ID
        ImageView clickDelete = (ImageView) viewHolder.getView(R.id.clickDelete);
        //show dialog to validate song deletion on click
        clickDelete.setOnClickListener(view -> musicListViewModel.deleteSong(context, song).show());
    }

    //play clicked song on music player
    public void onClick(MusicPlayer musicPlayer, int position){
        musicPlayer.setPlaylist(songList, position);
    }

    //show or hide hidden layout on long click
    public void onLongClick(int position){
        //get song on long click position
        Song song = songList.get(position);
        //invert hidden layout visibility
        buttonMap.put(song, Boolean.FALSE.equals(buttonMap.get(song)));
        //notify adapter dataset changed
        notifyDataSetChanged();
    }

    //update song list when song list changes
    public void updateSongList(List<Song> newSongList, String data, String order){
        //clear old song list
        songList.clear();
        //add new song list
        songList.addAll(newSongList);
        //sort new song list
        sortSongList(data, order);
    }

    //sort song list
    public void sortSongList(String data, String order){
        //sort song list
        musicListViewModel.sortSongList(songList, data, order);
        //hide hidden layout for all songs
        for(Song song : songList) buttonMap.put(song, false);
        //notify adapter dataset changed
        notifyDataSetChanged();
    }
}
