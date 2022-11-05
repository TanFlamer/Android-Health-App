package com.example.myapp.fragmentsSleep;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.example.myapp.R;
import com.example.myapp.fragmentsSleep.recyclerSleep.SleepRecyclerItem;
import com.example.myapp.fragmentsSleep.recyclerSleep.SleepRecyclerAdapter;
import com.example.myapp.mainActivities.Account;
import com.example.myapp.subActivities.DataSleep;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.util.ArrayList;
import java.util.List;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SleepList#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SleepList extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SleepList() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SleepList.
     */
    // TODO: Rename and change types and number of parameters
    public static SleepList newInstance(String param1, String param2) {
        SleepList fragment = new SleepList();
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
        return inflater.inflate(R.layout.fragment_sleep_list, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        RecyclerView recyclerView = requireView().findViewById(R.id.sleepRecyclerView);
        List<SleepRecyclerItem> sleepRecyclerItemList = new ArrayList<>();

        sleepRecyclerItemList.add(new SleepRecyclerItem("test", "test","test", 0));
        sleepRecyclerItemList.add(new SleepRecyclerItem( "test1", "test1","test1", 1));
        sleepRecyclerItemList.add(new SleepRecyclerItem("test", "test","test", 0));
        sleepRecyclerItemList.add(new SleepRecyclerItem( "test1", "test1","test1", 1));

        SleepRecyclerAdapter sleepRecyclerAdapter = new SleepRecyclerAdapter(getContext(), sleepRecyclerItemList);
        recyclerView.setAdapter(sleepRecyclerAdapter);
        recyclerView.setHasFixedSize(true);
        recyclerView.setLayoutManager(new LinearLayoutManager(getContext()));

        FloatingActionButton floatingActionButton = requireView().findViewById(R.id.buttonFloating);
        floatingActionButton.setOnClickListener(view1 -> {
            startActivity(new Intent(getContext(), DataSleep.class));
            getActivity().overridePendingTransition(0, 0);
        });
    }
}