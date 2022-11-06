package com.example.myapp.fragmentsSport;

import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CalendarView;
import android.widget.Toast;

import com.example.myapp.R;
import com.example.myapp.mainActivities.Save;
import com.example.myapp.subActivities.DataSport;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SportCalendar#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SportCalendar extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SportCalendar() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SportCalendar.
     */
    // TODO: Rename and change types and number of parameters
    public static SportCalendar newInstance(String param1, String param2) {
        SportCalendar fragment = new SportCalendar();
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
        return inflater.inflate(R.layout.fragment_sport_calendar, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        CalendarView calendarView = requireView().findViewById(R.id.calendarSport);
        calendarView.setOnDateChangeListener((calendarView1, i, i1, i2) -> {
            Toast.makeText(getContext(), i + " " + i1 + " " + i2, Toast.LENGTH_SHORT).show();
        });

        Button button = requireView().findViewById(R.id.calendarSportButton);
        button.setOnClickListener(view1 -> {
            startActivity(new Intent(getContext(), DataSport.class));
            getActivity().overridePendingTransition(0, 0);
        });
    }
}