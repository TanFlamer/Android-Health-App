package com.example.myapp.fragmentsMusic;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ListView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.example.myapp.R;
import com.example.myapp.fragmentsMusic.listMusic.MusicListAdapter;
import com.example.myapp.fragmentsMusic.listMusic.MusicListItem;
import com.example.myapp.subActivities.DataMusic;
import com.example.myapp.subActivities.DataSleep;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.util.ArrayList;
import java.util.List;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link MusicList#newInstance} factory method to
 * create an instance of this fragment.
 */
public class MusicList extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public MusicList() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment Songs.
     */
    // TODO: Rename and change types and number of parameters
    public static MusicList newInstance(String param1, String param2) {
        MusicList fragment = new MusicList();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_music_list, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        ListView listView = requireView().findViewById(R.id.musicListView);
        List<MusicListItem> musicListItemList = new ArrayList<>();

        musicListItemList.add(new MusicListItem("test", 0));
        musicListItemList.add(new MusicListItem("test1", 1));

        MusicListAdapter musicListAdapter = new MusicListAdapter(getContext(), R.layout.music_list_item, musicListItemList);
        listView.setAdapter(musicListAdapter);

        FloatingActionButton floatingActionButton = requireView().findViewById(R.id.buttonFloating);
        floatingActionButton.setOnClickListener(view1 -> {
            startActivity(new Intent(getContext(), DataMusic.class));
            getActivity().overridePendingTransition(0, 0);
        });
    }
}